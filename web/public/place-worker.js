// The worker's half of the GPU bridge.
//
// Loon's VM runs here, synchronously, because that is what it is. When a
// program performs a `Place` operation that needs the GPU, the bridge below
// posts the request to the main thread and blocks on `Atomics.wait` until the
// reply arrives. Blocking is only legal off the main thread, which is the
// entire reason this file exists.

import init, { eval_placed, init_gpu_bridge } from '/loon_wasm.js';

const STATE = 0;
const STATUS = 1;
const LENGTH = 2;
const HEADER_WORDS = 4;

let words = null;
let payload = null;
const decoder = new TextDecoder();

/// Perform one device operation and block until the main thread answers.
function bridge(op, req) {
  if (!words) throw new Error('the GPU bridge is not connected');

  // Structured clone moves the request; typed arrays inside it are copied.
  const message = { kind: 'gpu', op };
  if (req) {
    if (req.bytes) message.bytes = req.bytes.slice();
    if (req.scalars) message.scalars = Array.from(req.scalars);
    if (req.buffers) message.buffers = Array.from(req.buffers);
    for (const k of ['id', 'n', 'shader', 'entry', 'dtype', 'byteLength']) {
      if (req[k] !== undefined) message[k] = req[k];
    }
  }

  Atomics.store(words, STATE, 0);
  self.postMessage(message);

  // The main thread is doing something asynchronous; wait for it.
  Atomics.wait(words, STATE, 0);

  const status = Atomics.load(words, STATUS);
  const len = Atomics.load(words, LENGTH);
  const bytes = payload.slice(0, len);
  if (status !== 0) {
    throw new Error(decoder.decode(bytes) || 'the device reported a failure');
  }
  return bytes;
}

let ready = null;

self.addEventListener('message', async (e) => {
  const data = e.data;
  if (!data) return;

  if (data.kind === 'gpu-ready') {
    words = new Int32Array(data.control, 0, HEADER_WORDS);
    payload = new Uint8Array(data.control, HEADER_WORDS * 4);
    return;
  }

  if (data.kind === 'run') {
    try {
      if (!ready) {
        ready = init().then(() => {
          init_gpu_bridge(bridge);
        });
      }
      await ready;
      const out = eval_placed(data.program, data.place);
      self.postMessage({ kind: 'result', id: data.id, ok: true, out });
    } catch (err) {
      self.postMessage({
        kind: 'result',
        id: data.id,
        ok: false,
        out: String(err && err.message ? err.message : err),
      });
    }
  }
});
