# Placement Is an Effect

*August 2026*

Last month I wrote that a syscall is an effect, and so the kernel is just the outermost handler. This month someone handed me a paper about running Rust on GPUs, and I spent a week discovering that the same sentence has another half.

The paper is [GPU Offload in Rust: Portable, Safe, and Fast](https://arxiv.org/abs/2608.13759), by Drehwald, Domínguez, Sala, Aspuru-Guzik, and Doerfert. It is good work and you should read it. They put GPU compilation inside `rustc`, they get data direction out of `&T` versus `&mut T` instead of making you write pragmas, and they measure themselves honestly against hand-written CUDA. My interest is in the part they are candid about not having solved, because it turns out to be a shape I recognized.

## The 400x

Their convenient interface looks like this:

```rust
offload!(vec_add, &a, &b, &mut c);
```

You write that, and the compiler transfers `a` and `b` to the GPU, runs the kernel, and copies `c` back. Lovely. Now put it in a loop, and every iteration ships the same two arrays across the bus again. They measure this at **up to 400x slower** than doing the transfers by hand.

So they add a second interface. You wrap your data in a `Preload` or `PreloadMut`, which pins it on the device and marks the point where it comes home with the value's `drop`. That works, and it costs you an annotation at every site. And then, to make the *convenient* interface fast too, they prototype a transfer-hoisting pass inside LLVM — loop-invariant code motion for `memcpy`s, essentially — which is still future work at the time of writing.

There is one benchmark they can't fix that way. It's called Energy: six kernels sharing about fifteen arrays, each with a few of its own. Keeping everything resident might blow past the device's memory; keeping nothing resident is the 400x. Deciding needs a heuristic, and they say plainly that they didn't ship one.

I read that and thought: that isn't a missing optimization. That's a missing *seam*.

## The seam

Here's the thing the compiler is trying to recover. It wants to know two facts:

1. What does this launch touch?
2. When does the host actually want to look?

And it can't just ask, because in Rust neither fact is written down anywhere. A launch is a function call. A host read is... a host read, some ordinary expression somewhere in the program that happens to name the same variable. So the compiler goes and reconstructs both from dataflow analysis, and where analysis fails, `Preload` makes you write the answer down by hand.

But both facts are *events*. Things that happen, in an order, that somebody might want to intercept.

We have a way to spell that.

```
[Place.run saxpy n #[3.0 x y out]]
[Place.read out]
```

`Place.run` is an effect operation. `Place.read` is an effect operation. Not a function, not a method on a smart pointer — an operation, which floats up until some handler catches it. And once those two are effects, the two questions the LLVM pass was reverse-engineering are just *arguments to a handler clause*.

## Nine lines

Here is the residency policy. It is in `os/place.oo`. It is not privileged, it is not in the compiler, and you could have written it.

```
[fn place/resident [thunk]
  [handle [thunk]
    [Place.run k n args]
      [do [Place.pin args]
          [resume [Place.run k n args]]]
    [Place.read b]
      [do [let v [Place.read b]]
          [Place.unpin b]
          [resume v]]]]]
```

Read it slowly, because it is doing the whole job. It catches every launch, tells the device to keep whatever that launch touched, and forwards the launch outward unchanged. It catches every read, lets the read happen, and releases the pin. That's it. `Place.pin` is the entire vocabulary — it means "this will be wanted again."

Wrap it around a program and:

```
loon run os/demo-residency.oo --place gpu

eight launches over one buffer
  answer #[8 8 8 8]
  no policy: uploads 8, resident hits 0, bytes in 128
  answer #[8 8 8 8]
  place/resident: uploads 1, resident hits 7, bytes in 16
```

Same program both times. Same answer both times. Eight uploads became one, because a handler said so.

On an actual GPU — this is an M4 Max, through Metal, via wgpu — the wall clock follows:

| launches | no policy | place/resident | speedup |
|---------:|----------:|---------------:|--------:|
| 8 | 29.3 ms | 8.4 ms | 3.5x |
| 32 | 94.8 ms | 11.2 ms | 8.4x |
| 128 | 358.8 ms | 18.1 ms | 19.9x |

The gap grows with the chain, which is exactly the paper's curve. The difference is where the fix lives. Theirs is an LLVM pass and a type. Mine is a `handle` form you can read in one sitting, change without rebuilding a compiler, and — this is the part I keep coming back to — *replace with a different one* when your program has different needs.

Which brings us to Energy.

## The heuristic they didn't ship

The Energy case is only hard if there's exactly one policy and it has to be right for everybody. When residency is a handler, "keep everything" and "keep these" are two handlers, and choosing between them is a line of code rather than a compiler flag nobody can change:

```
[place/resident-only #[e-new p-new q-new] work]
```

That's the heuristic. It's an argument. I am not claiming I solved their benchmark — I haven't run RAJAPerf and I'm not going to pretend otherwise. I'm claiming the thing they needed a heuristic *for* is, in this design, a place where the user gets to put one.

## Nobody wrote `&mut`

Here's the part I'm smug about. This is a kernel:

```
[kernel saxpy [i a x y out]
  [put out i [+ [* a [at x i]] [at y i]]]]
```

`x` and `y` are inputs. `out` has to come home. Nothing in that source says so.

Loon already had an ownership pass that figures out, for every function parameter, whether the body reads it, writes through it, or consumes it — the same distinction Rust makes you spell as `&T` / `&mut T` / `T`. It was computing that, using it for error messages, and throwing it away. Now it rides into the IR, and the placement layer reads it: `at` is a read, `put` is a write-through, so `out` is the argument that needs synchronizing back. The paper reads exactly the same fact off exactly the same distinction. They just make you type it.

The emitted shader gets it right down to the binding:

```wgsl
@group(0) @binding(1) var<storage, read> b1: array<f32>;
@group(0) @binding(2) var<storage, read> b2: array<f32>;
@group(0) @binding(3) var<storage, read_write> b3: array<f32>;
```

`read` versus `read_write`, decided by whether the kernel body said `at` or `put`.

## What a kernel isn't

Kernels are restricted. No closures, no allocation, no strings, no effects. Try it and the compiler names what it found:

```
kernel 'k' contains the effect operation 'IO.println'
  why: a kernel runs where there is no handler tower to perform effects against
```

That restriction is the safety argument, and it's where I think this design earns its keep against theirs. Their kernels can receive a slice and index it however they like, so "threads touch disjoint elements" has to be *promised* — by an `unsafe impl` of a partitioning strategy. Here a kernel receives an index and writes at that index. The unsafe program isn't rejected; it's unwriteable.

I'll take a restriction over a promise. A promise is a place where someone will eventually be wrong.

## Testing a GPU program without a GPU

This is the part the paper doesn't have a section for, and I don't think that's an oversight so much as a consequence: if launches aren't events, there's nothing to record.

```
loon run samples/place/saxpy.oo --place gpu --record trace.oo
loon replay trace.oo samples/place/saxpy.oo
```

The second command runs on a build with no GPU support compiled into it at all, and prints the same thing. From the program's point of view a kernel launch was an operation that returned nothing and a read was one that produced some numbers, so recording those *is* the run.

`Place.stats` is deliberately excluded from the recording. It reports on the run currently happening, and a replayed run genuinely moved no bytes — feeding back the original transfer counts would be a recording that lies about the execution it's part of. The replayed run says zero launches, because it performed zero launches.

And since a handler can decline to forward at all, "run this without a device" is four lines:

```
[fn place/dry-run [thunk]
  [handle [thunk]
    [Place.run k n args] [resume []]
    [Place.read b]       [resume #[]]]]
```

Every accounting number, no execution. And strace-for-GPU is the same shape as strace-for-syscalls was last month — perform the operation you intercepted, print on the way past.

## Where it runs

Metal on this laptop, today. Vulkan on a Linux box and DX12 on Windows are the same code path through wgpu, and I have not run either, so take them as "should" rather than "does."

And the browser, which I want to describe carefully because getting there was the strangest part.

Programs run in a tab, on the actual GPU:

```
placed on gpu: 4 launches over 32 work items;
               9 uploads (288 B), 3 downloads (96 B), 3 resident hits
```

That is WebGPU, driven by the same WGSL a desktop build hands to wgpu, with the residency handler deciding what gets copied.

Here is the strange part. WebGPU is asynchronous — you get a device from a promise and read a buffer back through `mapAsync`. Loon's VM is synchronous all the way down; `Place.read` is an effect operation that returns a value, not a future. Those two facts cannot both hold on one thread.

So they hold on two. The VM runs in a Web Worker, and every device call posts a request to the main thread and then blocks on `Atomics.wait` until the answer lands in a `SharedArrayBuffer`. The main thread, where the promises live, does the WebGPU work and wakes the worker. The blocking is real, and it is the whole trick: it lets an asynchronous API sit underneath a synchronous language without either one pretending to be the other.

None of that reached the VM. It sees an `eir::device::Device` — six operations: name, ensure-resident, is-resident, dispatch, download, evict — and wgpu implements it on a laptop while a JavaScript bridge implements it in a tab. Which is the same move placement makes at the language level, one floor down: the thing that varies goes behind an interface, and the code above does not change when the answer does.

The cost is a requirement for cross-origin isolation, since `SharedArrayBuffer` needs COOP/COEP headers. Without them the page says so and the other placements still work.

I said in a first draft that getting rid of that would need an asynchronous effect path in the VM. Then I tried it, and it turns out the VM already has one — it just isn't called that.

A handler clause does not have to call `resume`. If it hands `resume` somewhere else and returns, the handled computation *unwinds*, and the continuation is still live in whoever caught it. Call it later and the program picks up mid-expression, exactly where it stopped:

```
work: starting
host: computation parked; the rest of it is mine now
host: ...doing something slow...
work: continued with 21
host: finished with 42
```

That's `os/demo-park.oo`, and it is the entire mechanism an asynchronous host needs. A browser can't answer `Place.read` immediately — reading a GPU buffer back is a promise — but it doesn't have to answer immediately. It can take the continuation, go away, and come back when the bytes arrive. Uploads and dispatches need none of this, because `writeBuffer` and `submit` are already synchronous.

What's left there is plumbing: a VM that outlives one call, since the continuation lives in its heap, and an export for the page to resume through. Not semantics. I had assumed the hard part was the language and the easy part was the wiring, and it was the other way round — which is what I get for writing down what I thought was true instead of trying it.

Every kernel in the repo is parsed and type-checked by naga in CI, on machines with no GPU. That's the automated cross-target validation the paper says is still missing — they found a host/device divergence in slice lowering by hand, `(ptr, len)` on two targets and `[i64; 2]` on a third. We have the same class of hazard: NaN-boxing constants that used to be copy-pasted into three backends under a comment asking the next person to keep them in sync. They now live in one file, and a conformance test compiles the same literals on every backend and compares raw bits.

That test found three real divergences the first time it ran, including one where `loon run --native` silently returned `()` for any program with a `main` function. Which is a good argument for writing the test.

## The row I didn't expect

Once kernels stopped going through the interpreter — there's a typed executor now that runs the numeric subset against raw slices — I added `--place par`, which splits the index range across cores. Each thread gets a disjoint piece of the output from `split_at_mut`, so "threads touch disjoint elements" isn't promised by an `unsafe impl`; it's what the borrow checker hands back.

Then I ran the same kernel four ways:

| elements | cpu | par | gpu |
|---------:|----:|----:|----:|
| 1,024 | 447 µs | 566 µs | 10.3 ms |
| 262,144 | 8.9 ms | 3.3 ms | 12.4 ms |
| 1,048,576 | 36.4 ms | 11.4 ms | 19.2 ms |

Every core beats the GPU at a million elements. This machine has a lot of fast ones, and a launch pays submission and transfer before it computes anything.

I like this result more than I'd like a win. Where the crossover sits is a property of the machine, not of the program — and I found it by changing one word on a command line, because the program genuinely does not know where it runs. If placement were a compile-time decision I'd have had to rebuild something to ask the question, and I probably wouldn't have bothered.

One more thing falls out of making `Place.read` the only way to get data back: launches don't block on each other. A dispatch submits and returns; nothing waits until the host asks. Sixty-four launches take 19.9 ms against 6.9 ms for one — if each waited, that would be closer to 440. The paper prototypes asynchronous transfers as a separate optimization. Here it's just what happens when the synchronization point is a thing the program says out loud.

## What I'm not claiming

We do not beat hand-written CUDA. We haven't measured against it and we're not going to imply otherwise. The CPU column in our benchmarks is Loon's own interpreter — the slowest honest baseline — so "3.1x faster on the GPU at 262k elements" means *there is a lot to gain by leaving the interpreter*, not anything about generated code quality.

Reductions and atomics are outside the kernel subset. WGSL core has no 64-bit scalar, so an f64 buffer is computed in f32 on the device and we report the narrowing rather than hiding it.

And the honest summary of the whole comparison: they built a compiler and I moved a seam. Those are different kinds of work. The reason I think the seam is worth the post is that it makes a class of thing — residency, prefetch, eviction, tracing, simulation, replay — stop being compiler features that someone has to ship for you, and start being ordinary code that you can write on a Tuesday.

## The arc

v0.7: effects, end to end. v0.8: syscalls are effects, so the kernel is the outermost handler. v0.9: placement is an effect, so the GPU is a handler in the middle.

I don't have a fourth one yet. But I've stopped being surprised when something that looked like it needed a compiler pass turns out to need a `handle`.

---

Try it:

```
loon run os/demo-place.oo                    # one program, four handlers
loon run os/demo-residency.oo --place device # the transfer gap
loon run samples/place/saxpy.oo --place gpu  # on real hardware
```
