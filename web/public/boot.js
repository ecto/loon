// boot.js — Loon website bootstrap
// Loads WASM interpreter, sets up DOM bridge, evaluates Loon source.

import init, { init_dom_bridge, eval_ui, eval_program } from './loon_wasm.js';

// --- DOM Bridge ---
// The Loon interpreter calls dom/* builtins which route through this bridge.
// We maintain a node table mapping integer handles to DOM nodes.

const nodes = [document]; // handle 0 = document
let nextHandle = 1;

function allocHandle(node) {
  const h = nextHandle++;
  nodes[h] = node;
  return h;
}

function getNode(handle) {
  return nodes[handle];
}

// Event listener storage for cleanup
const listeners = new Map();
let nextListenerId = 1;

function domBridge(op, args) {
  switch (op) {
    case 'createElement': {
      const el = document.createElement(args[0]);
      return allocHandle(el);
    }
    case 'createText': {
      const t = document.createTextNode(args[0]);
      return allocHandle(t);
    }
    case 'setAttribute': {
      const node = getNode(args[0]);
      if (node) node.setAttribute(args[1], args[2]);
      return null;
    }
    case 'setStyle': {
      const node = getNode(args[0]);
      if (node) node.style[args[1]] = args[2];
      return null;
    }
    case 'appendChild': {
      const parent = getNode(args[0]);
      const child = getNode(args[1]);
      if (parent && child) parent.appendChild(child);
      return null;
    }
    case 'removeChild': {
      const parent = getNode(args[0]);
      const child = getNode(args[1]);
      if (parent && child) parent.removeChild(child);
      return null;
    }
    case 'replaceChild': {
      const parent = getNode(args[0]);
      const newChild = getNode(args[1]);
      const oldChild = getNode(args[2]);
      if (parent && newChild && oldChild) parent.replaceChild(newChild, oldChild);
      return null;
    }
    case 'setText': {
      const node = getNode(args[0]);
      if (node) node.textContent = args[1];
      return null;
    }
    case 'setInnerHTML': {
      const node = getNode(args[0]);
      if (node) node.innerHTML = args[1];
      return null;
    }
    case 'querySelector': {
      const el = document.querySelector(args[0]);
      if (!el) return null;
      // Check if already tracked
      const existing = nodes.indexOf(el);
      if (existing >= 0) return existing;
      return allocHandle(el);
    }
    case 'addListener': {
      const node = getNode(args[0]);
      const event = args[1];
      const callbackId = args[2]; // callback handle from Loon
      if (!node) return null;
      const id = nextListenerId++;
      const handler = (e) => {
        // Call back into Loon via a global callback dispatcher
        if (window.__loon_event_handler) {
          window.__loon_event_handler(callbackId, event, e);
        }
      };
      node.addEventListener(event, handler);
      listeners.set(id, { node, event, handler });
      return id;
    }
    case 'removeListener': {
      const entry = listeners.get(args[0]);
      if (entry) {
        entry.node.removeEventListener(entry.event, entry.handler);
        listeners.delete(args[0]);
      }
      return null;
    }
    case 'setTitle': {
      document.title = args[0];
      return null;
    }
    case 'pushState': {
      history.pushState(null, '', args[0]);
      // Dispatch popstate-like event for router
      window.dispatchEvent(new CustomEvent('loon:navigate', { detail: { path: args[0] } }));
      return null;
    }
    case 'location': {
      return window.location.pathname;
    }
    case 'requestAnimationFrame': {
      // args[0] is a callback handle
      const cbId = args[0];
      requestAnimationFrame(() => {
        if (window.__loon_raf_handler) {
          window.__loon_raf_handler(cbId);
        }
      });
      return null;
    }
    case 'setTimeout': {
      const cbId = args[0];
      const ms = args[1];
      setTimeout(() => {
        if (window.__loon_timeout_handler) {
          window.__loon_timeout_handler(cbId);
        }
      }, ms);
      return null;
    }
    default:
      console.warn('Unknown DOM bridge op:', op, args);
      return null;
  }
}

// --- Loon Source ---
// All .loon source files are bundled as string constants.
// In dev, we fetch them; in prod, they're inlined by the build script.

async function loadSource(path) {
  const resp = await fetch(path);
  if (!resp.ok) throw new Error(`Failed to load ${path}: ${resp.status}`);
  return resp.text();
}

// --- Boot ---

async function boot() {
  try {
    // Initialize WASM
    await init();

    // Set up DOM bridge
    init_dom_bridge(domBridge);

    // Load and evaluate Loon source files in order
    const sources = [
      'src/ui.loon',
      'src/router.loon',
      'src/components/nav.loon',
      'src/components/footer.loon',
      'src/components/code.loon',
      'src/components/editor.loon',
      'src/pages/home.loon',
      'src/pages/tour.loon',
      'src/pages/install.loon',
      'src/pages/play.loon',
      'src/pages/blog.loon',
      'src/pages/roadmap.loon',
      'src/pages/community.loon',
      'src/app.loon',
    ];

    // Load all sources
    const loaded = {};
    for (const src of sources) {
      loaded[src] = await loadSource(src);
    }

    // Concatenate and evaluate as one program
    const fullSource = sources.map(s => `; --- ${s} ---\n${loaded[s]}`).join('\n\n');
    eval_ui(fullSource);

    console.log('Loon website booted successfully');
  } catch (e) {
    console.error('Loon boot failed:', e);
    const app = document.getElementById('app');
    if (app) {
      app.innerHTML = `<div style="padding: 2rem; font-family: monospace;">
        <h1>Loon failed to start</h1>
        <pre style="color: #c00;">${e.message || e}</pre>
      </div>`;
    }
  }
}

// Make eval_program available globally for the playground
window.__loon_eval = eval_program;

boot();
