//! Running a kernel on an actual GPU.
//!
//! Everything up to here has been about *where* a kernel could run. This is
//! the part that puts one on hardware: the WGSL from `eir::wgsl` becomes a
//! compute pipeline, buffers become GPU allocations, and a dispatch happens.
//!
//! Through wgpu, so the same kernel reaches Metal on this machine, Vulkan on a
//! Linux box, DX12 on Windows, and WebGPU in a browser tab. That last one is
//! the interesting entry in the list: it is a target a compiler emitting PTX
//! and AMDGCN cannot reach at all, and it comes free from having chosen a
//! portable shading language rather than a vendor's.
//!
//! The device is discovered lazily and its absence is not an error. A machine
//! with no GPU runs the same programs through the CPU path, so nothing in the
//! test suite depends on hardware being present — it only gets *checked* on
//! hardware when hardware is there.

use super::layout::DType;
use super::vm::{BufData, Buffer};
use super::wgsl::ArgKind;

/// A GPU we can dispatch to.
pub struct Gpu {
    device: wgpu::Device,
    queue: wgpu::Queue,
    name: String,
    /// Buffers that currently live on the device, keyed by the host's heap
    /// slot.
    ///
    /// This is what makes residency real rather than modelled. Without it,
    /// every launch would upload its arguments again and copy its results back
    /// again, and a residency policy would be describing a saving that did not
    /// happen. With it, `Place.pin` keeps an allocation alive across launches
    /// and `Place.read` is the only thing that moves bytes home.
    resident: std::cell::RefCell<std::collections::HashMap<usize, wgpu::Buffer>>,
    /// Compiled pipelines, keyed by shader source.
    ///
    /// Compiling a shader takes on the order of a millisecond, which is
    /// hundreds of times what a small launch costs. Without this cache a loop
    /// that launches the same kernel repeatedly — the shape every offload
    /// benchmark has, and the shape a residency policy exists to serve — pays
    /// to recompile it on every iteration, and the GPU loses to an
    /// interpreter for entirely uninteresting reasons.
    pipelines: std::cell::RefCell<std::collections::HashMap<String, CachedPipeline>>,
}

/// A shader that has already been compiled.
struct CachedPipeline {
    pipeline: wgpu::ComputePipeline,
    layout: wgpu::BindGroupLayout,
}

/// What went wrong, in a sentence a person can act on.
#[derive(Debug, Clone)]
pub struct Error(pub String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for Error {}

/// One argument, as the GPU will see it.
pub enum GpuArg<'a> {
    /// A buffer, uploaded before the dispatch and read back after if written.
    Buffer { data: &'a Buffer, writable: bool },
    /// A number, packed into the uniform block.
    Scalar(f32),
}

impl Gpu {
    /// Find a GPU, or explain why there isn't one.
    ///
    /// Called once and cached by the caller: adapter enumeration is slow
    /// enough that doing it per launch would dominate any kernel worth
    /// offloading.
    pub fn open() -> Result<Gpu, Error> {
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: None,
            force_fallback_adapter: false,
        }))
        .map_err(|e| Error(format!("no GPU adapter available: {e}")))?;

        let info = adapter.get_info();
        let name = format!("{} ({:?})", info.name, info.backend);

        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            label: Some("loon"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::downlevel_defaults(),
            memory_hints: wgpu::MemoryHints::Performance,
            trace: wgpu::Trace::Off,
        }))
        .map_err(|e| Error(format!("could not open the GPU device: {e}")))?;

        Ok(Gpu {
            device,
            queue,
            name,
            resident: std::cell::RefCell::new(std::collections::HashMap::new()),
            pipelines: std::cell::RefCell::new(std::collections::HashMap::new()),
        })
    }

    /// A human-readable name for the device, for `--place-stats` and errors.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Run `shader` over `n` work items with `args`.
    ///
    /// Returns the bytes read back from each writable buffer, paired with the
    /// argument position it came from. The caller decides what to do with
    /// them — which keeps this function free of any opinion about where the
    /// host's copy lives, and makes the transfer itself something a test can
    /// look at directly.
    ///
    /// Blocking: the dispatch is submitted and awaited before returning. A
    /// non-blocking form belongs with the scheduler, not here.
    pub fn run(
        &self,
        shader: &str,
        entry: &str,
        n: u32,
        args: &[GpuArg<'_>],
    ) -> Result<Vec<(usize, Vec<u8>)>, Error> {
        self.ensure_pipeline(shader, entry);

        // The uniform block is `n` followed by every scalar argument, in order
        // — matching what the emitter wrote into `struct Params`.
        let mut uniform: Vec<u8> = (n as i32).to_le_bytes().to_vec();
        for arg in args.iter() {
            if let GpuArg::Scalar(v) = arg {
                uniform.extend_from_slice(&v.to_le_bytes());
            }
        }
        // WGSL requires a uniform buffer to be a multiple of 16 bytes.
        while uniform.len() % 16 != 0 {
            uniform.push(0);
        }
        let uniform_buf = self.create_buffer(
            "params",
            &uniform,
            wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        );

        // Upload every buffer argument, remembering which ones have to come
        // back and how big they are.
        let mut storage: Vec<wgpu::Buffer> = Vec::new();
        let mut readback: Vec<(usize, u64)> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            if let GpuArg::Buffer { data, writable } = arg {
                let bytes = data.to_bytes();
                let usage = wgpu::BufferUsages::STORAGE
                    | wgpu::BufferUsages::COPY_DST
                    | wgpu::BufferUsages::COPY_SRC;
                let buf = self.create_buffer(&format!("b{i}"), &bytes, usage);
                if *writable {
                    readback.push((i, bytes.len() as u64));
                }
                storage.push(buf);
            }
        }

        // Bindings: 0 is the uniform block, storage buffers follow in order.
        let mut entries: Vec<wgpu::BindGroupEntry> = vec![wgpu::BindGroupEntry {
            binding: 0,
            resource: uniform_buf.as_entire_binding(),
        }];
        for (slot, buf) in storage.iter().enumerate() {
            entries.push(wgpu::BindGroupEntry {
                binding: slot as u32 + 1,
                resource: buf.as_entire_binding(),
            });
        }

        let cache = self.pipelines.borrow();
        let cached = cache.get(shader).expect("pipeline was just ensured");

        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("args"),
            layout: &cached.layout,
            entries: &entries,
        });

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: Some("run") });
        {
            let mut pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: Some(entry),
                timestamp_writes: None,
            });
            pass.set_pipeline(&cached.pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            // The shader's workgroup size is 64; round up and let the bounds
            // check in the shader discard the overshoot.
            pass.dispatch_workgroups(n.div_ceil(64).max(1), 1, 1);
        }

        // Copy every written buffer into a staging buffer the host can map.
        let staging: Vec<(usize, wgpu::Buffer)> = readback
            .iter()
            .map(|(i, size)| {
                let s = self.device.create_buffer(&wgpu::BufferDescriptor {
                    label: Some("readback"),
                    size: *size,
                    usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
                    mapped_at_creation: false,
                });
                (*i, s)
            })
            .collect();
        for ((_, size), (slot, dst)) in readback.iter().zip(staging.iter()) {
            let src = storage_for(args, *slot, &storage);
            encoder.copy_buffer_to_buffer(src, 0, dst, 0, *size);
        }

        self.queue.submit(Some(encoder.finish()));
        drop(cache);

        // Map each staging buffer and collect what came back.
        let mut results: Vec<(usize, Vec<u8>)> = Vec::new();
        for (slot, dst) in &staging {
            let slice = dst.slice(..);
            let (tx, rx) = std::sync::mpsc::channel();
            slice.map_async(wgpu::MapMode::Read, move |r| {
                let _ = tx.send(r);
            });
            self.device
                .poll(wgpu::PollType::Wait)
                .map_err(|e| Error(format!("waiting for the GPU: {e:?}")))?;
            match rx.recv() {
                Ok(Ok(())) => {}
                Ok(Err(e)) => return Err(Error(format!("mapping results: {e:?}"))),
                Err(e) => return Err(Error(format!("the GPU never reported back: {e}"))),
            }
            let bytes = slice.get_mapped_range().to_vec();
            dst.unmap();
            results.push((*slot, bytes));
        }

        Ok(results)
    }

    /// Ensure `id`'s data is on the device, uploading it if it is not already.
    ///
    /// Returns whether an upload actually happened, so the caller's accounting
    /// reflects what the hardware did rather than what the model predicted.
    pub fn ensure_resident(&self, id: usize, buf: &Buffer) -> bool {
        if self.resident.borrow().contains_key(&id) {
            return false;
        }
        let bytes = buf.to_bytes();
        let gpu_buf = self.create_buffer(
            &format!("buf{id}"),
            &bytes,
            wgpu::BufferUsages::STORAGE
                | wgpu::BufferUsages::COPY_DST
                | wgpu::BufferUsages::COPY_SRC,
        );
        self.resident.borrow_mut().insert(id, gpu_buf);
        true
    }

    /// Whether `id` is currently on the device.
    pub fn is_resident(&self, id: usize) -> bool {
        self.resident.borrow().contains_key(&id)
    }

    /// Drop a device allocation.
    pub fn evict(&self, id: usize) {
        self.resident.borrow_mut().remove(&id);
    }

    /// Copy a resident buffer's contents back to the host.
    ///
    /// This is the only path by which device data becomes visible again, which
    /// is exactly what makes `Place.read` the synchronization point a
    /// residency handler can reason about.
    pub fn download(&self, id: usize, byte_len: usize) -> Result<Vec<u8>, Error> {
        let resident = self.resident.borrow();
        let Some(src) = resident.get(&id) else {
            return Err(Error(format!("buffer {id} is not on the device")));
        };
        let staging = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("readback"),
            size: byte_len.max(4) as u64,
            usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("download"),
            });
        encoder.copy_buffer_to_buffer(src, 0, &staging, 0, byte_len.max(4) as u64);
        self.queue.submit(Some(encoder.finish()));

        let slice = staging.slice(..);
        let (tx, rx) = std::sync::mpsc::channel();
        slice.map_async(wgpu::MapMode::Read, move |r| {
            let _ = tx.send(r);
        });
        self.device
            .poll(wgpu::PollType::Wait)
            .map_err(|e| Error(format!("waiting for the GPU: {e:?}")))?;
        match rx.recv() {
            Ok(Ok(())) => {}
            Ok(Err(e)) => return Err(Error(format!("mapping results: {e:?}"))),
            Err(e) => return Err(Error(format!("the GPU never reported back: {e}"))),
        }
        let mut bytes = slice.get_mapped_range().to_vec();
        staging.unmap();
        bytes.truncate(byte_len);
        Ok(bytes)
    }

    /// Dispatch a shader over buffers that are already resident.
    ///
    /// Nothing is uploaded and nothing is read back: the caller decides when
    /// data moves, which is the whole point of making placement a handler's
    /// decision rather than a launch's side effect.
    pub fn dispatch(
        &self,
        shader: &str,
        entry: &str,
        n: u32,
        scalars: &[f32],
        buffers: &[usize],
    ) -> Result<(), Error> {
        self.ensure_pipeline(shader, entry);

        let mut uniform: Vec<u8> = (n as i32).to_le_bytes().to_vec();
        for v in scalars {
            uniform.extend_from_slice(&v.to_le_bytes());
        }
        while uniform.len() % 16 != 0 {
            uniform.push(0);
        }
        let uniform_buf = self.create_buffer(
            "params",
            &uniform,
            wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        );

        let resident = self.resident.borrow();
        let mut entries: Vec<wgpu::BindGroupEntry> = vec![wgpu::BindGroupEntry {
            binding: 0,
            resource: uniform_buf.as_entire_binding(),
        }];
        for (slot, id) in buffers.iter().enumerate() {
            let buf = resident
                .get(id)
                .ok_or_else(|| Error(format!("buffer {id} is not on the device")))?;
            entries.push(wgpu::BindGroupEntry {
                binding: slot as u32 + 1,
                resource: buf.as_entire_binding(),
            });
        }

        let cache = self.pipelines.borrow();
        let cached = cache.get(shader).expect("pipeline was just ensured");
        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("args"),
            layout: &cached.layout,
            entries: &entries,
        });

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: Some("run") });
        {
            let mut pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: Some(entry),
                timestamp_writes: None,
            });
            pass.set_pipeline(&cached.pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.dispatch_workgroups(n.div_ceil(64).max(1), 1, 1);
        }
        self.queue.submit(Some(encoder.finish()));
        Ok(())
    }

    /// Compile `shader` if this is the first time we have seen it.
    fn ensure_pipeline(&self, shader: &str, entry: &str) {
        if self.pipelines.borrow().contains_key(shader) {
            return;
        }
        let module = self
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some(entry),
                source: wgpu::ShaderSource::Wgsl(shader.into()),
            });
        let pipeline = self
            .device
            .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                label: Some(entry),
                layout: None,
                module: &module,
                entry_point: Some(entry),
                compilation_options: Default::default(),
                cache: None,
            });
        let layout = pipeline.get_bind_group_layout(0);
        self.pipelines
            .borrow_mut()
            .insert(shader.to_string(), CachedPipeline { pipeline, layout });
    }

    /// How many distinct shaders have been compiled so far.
    pub fn compiled_shaders(&self) -> usize {
        self.pipelines.borrow().len()
    }

    fn create_buffer(&self, label: &str, bytes: &[u8], usage: wgpu::BufferUsages) -> wgpu::Buffer {
        use wgpu::util::DeviceExt as _;
        // An empty buffer is invalid in wgpu; give it one padded element.
        let padded;
        let contents = if bytes.is_empty() {
            padded = vec![0u8; 4];
            &padded[..]
        } else {
            bytes
        };
        self.device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some(label),
                contents,
                usage,
            })
    }
}

/// The storage buffer belonging to argument `slot`.
///
/// Scalars occupy no binding, so the storage list is indexed by *buffer*
/// position rather than argument position — the same numbering the emitter
/// used, kept in one place so the two cannot drift.
fn storage_for<'a>(
    args: &[GpuArg<'_>],
    slot: usize,
    storage: &'a [wgpu::Buffer],
) -> &'a wgpu::Buffer {
    let idx = args
        .iter()
        .take(slot)
        .filter(|a| matches!(a, GpuArg::Buffer { .. }))
        .count();
    &storage[idx]
}

/// Copy raw device bytes into a host buffer, converting if the buffer's
/// element type is wider than what the GPU computed.
///
/// WGSL has no f64 or i64, so a 64-bit Loon buffer is computed in 32 bits on
/// the device. Widening on the way back is the honest completion of that: the
/// values really were f32, and pretending otherwise would hide precision the
/// program did not get.
fn write_back(buf: &mut Buffer, bytes: &[u8]) {
    match &mut buf.data {
        BufData::F32(v) => {
            for (slot, chunk) in v.iter_mut().zip(bytes.chunks_exact(4)) {
                *slot = f32::from_le_bytes(chunk.try_into().unwrap());
            }
        }
        BufData::I32(v) => {
            for (slot, chunk) in v.iter_mut().zip(bytes.chunks_exact(4)) {
                *slot = i32::from_le_bytes(chunk.try_into().unwrap());
            }
        }
        BufData::F64(v) => {
            for (slot, chunk) in v.iter_mut().zip(bytes.chunks_exact(4)) {
                *slot = f32::from_le_bytes(chunk.try_into().unwrap()) as f64;
            }
        }
        BufData::I64(v) => {
            for (slot, chunk) in v.iter_mut().zip(bytes.chunks_exact(4)) {
                *slot = i32::from_le_bytes(chunk.try_into().unwrap()) as i64;
            }
        }
    }
}

/// The element type a buffer is given to the GPU as.
///
/// 64-bit buffers are narrowed, because WGSL core has no 64-bit scalar. This
/// is reported rather than assumed: see `DType::gpu_ok`.
pub fn device_dtype(d: DType) -> DType {
    match d {
        DType::F64 => DType::F32,
        DType::I64 => DType::I32,
        other => other,
    }
}

/// Convert a buffer to the 32-bit form the device will hold.
pub fn narrow(buf: &Buffer) -> Buffer {
    match &buf.data {
        BufData::F64(v) => Buffer {
            data: BufData::F32(v.iter().map(|x| *x as f32).collect()),
        },
        BufData::I64(v) => Buffer {
            data: BufData::I32(v.iter().map(|x| *x as i32).collect()),
        },
        _ => buf.clone(),
    }
}

/// Describe an argument for the emitter, given what it actually is.
pub fn arg_kind(arg: &GpuArg<'_>) -> ArgKind {
    match arg {
        GpuArg::Buffer { data, writable } => ArgKind::Buffer {
            dtype: device_dtype(data.dtype()),
            writable: *writable,
        },
        GpuArg::Scalar(_) => ArgKind::Scalar(DType::F32),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Open a GPU, or skip. A machine without one is not a failing machine.
    fn gpu_or_skip() -> Option<Gpu> {
        match Gpu::open() {
            Ok(g) => Some(g),
            Err(e) => {
                println!("SKIPPED: {e}");
                None
            }
        }
    }

    const DOUBLE: &str = r#"
struct Params { n: i32, s0: f32, };
@group(0) @binding(0) var<uniform> params: Params;
@group(0) @binding(1) var<storage, read_write> b0: array<f32>;
@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
    let idx: i32 = i32(gid.x);
    if (idx >= params.n) { return; }
    b0[u32(idx)] = b0[u32(idx)] * params.s0;
}
"#;

    #[test]
    fn a_kernel_runs_on_the_gpu_and_the_results_come_back() {
        let Some(gpu) = gpu_or_skip() else { return };
        println!("device: {}", gpu.name());

        let mut buf = Buffer {
            data: BufData::F32(vec![1.0, 2.0, 3.0, 4.0]),
        };
        let input = buf.clone();
        let args = vec![
            GpuArg::Scalar(3.0),
            GpuArg::Buffer {
                data: &input,
                writable: true,
            },
        ];
        let results = gpu.run(DOUBLE, "main", 4, &args).expect("dispatch");
        assert_eq!(results.len(), 1, "one writable buffer comes back");
        write_back(&mut buf, &results[0].1);
        assert_eq!(buf.data, BufData::F32(vec![3.0, 6.0, 9.0, 12.0]));
    }

    #[test]
    fn work_that_does_not_fill_a_workgroup_still_stays_in_bounds() {
        // A dispatch rounds up to whole workgroups, so a 3-element buffer runs
        // 64 invocations. The bounds check in the shader is what keeps the
        // other 61 from writing past the end.
        let Some(gpu) = gpu_or_skip() else { return };
        let mut buf = Buffer {
            data: BufData::F32(vec![1.0, 1.0, 1.0]),
        };
        let input = buf.clone();
        let args = vec![
            GpuArg::Scalar(5.0),
            GpuArg::Buffer {
                data: &input,
                writable: true,
            },
        ];
        let results = gpu.run(DOUBLE, "main", 3, &args).expect("dispatch");
        write_back(&mut buf, &results[0].1);
        assert_eq!(buf.data, BufData::F32(vec![5.0, 5.0, 5.0]));
    }

    #[test]
    fn a_read_only_buffer_is_not_copied_back() {
        // Only what the kernel writes has to travel home. This is the same
        // distinction the ownership pass draws, arriving at the hardware.
        let Some(gpu) = gpu_or_skip() else { return };
        let src = Buffer {
            data: BufData::F32(vec![1.0, 2.0]),
        };
        let args = vec![
            GpuArg::Scalar(2.0),
            GpuArg::Buffer {
                data: &src,
                writable: false,
            },
        ];
        // The shader writes b0, but we declared it read-only here, so nothing
        // is read back — the accounting follows the declaration.
        let results = gpu.run(DOUBLE, "main", 2, &args).expect("dispatch");
        assert!(results.is_empty(), "nothing was declared writable");
    }

    #[test]
    fn narrowing_is_explicit_about_what_the_device_can_hold() {
        assert_eq!(device_dtype(DType::F64), DType::F32);
        assert_eq!(device_dtype(DType::I64), DType::I32);
        assert_eq!(device_dtype(DType::F32), DType::F32);

        let wide = Buffer {
            data: BufData::F64(vec![1.5, 2.5]),
        };
        assert_eq!(narrow(&wide).data, BufData::F32(vec![1.5, 2.5]));
    }
}
