// boot.js — Loon website bootstrap
// Loads WASM interpreter, sets up DOM bridge, evaluates Loon source.
// Supports hot reload via SSE when served by the Loon dev server.

import init, { init_dom_bridge, eval_ui, eval_program, eval_with_output, invoke_callback } from '/loon_wasm.js';

// Lazy import — these may not exist in older WASM builds
let reset_runtime = () => {};
let eval_ui_checked = null;
let enable_effect_log = null;
let get_effect_log = null;
let clear_effect_log = null;

const wasmReady = import('/loon_wasm.js').then(m => {
  if (m.reset_runtime) reset_runtime = m.reset_runtime;
  if (m.eval_ui_checked) eval_ui_checked = m.eval_ui_checked;
  if (m.enable_effect_log) enable_effect_log = m.enable_effect_log;
  if (m.get_effect_log) get_effect_log = m.get_effect_log;
  if (m.clear_effect_log) clear_effect_log = m.clear_effect_log;
});

// --- Source file list ---
const SOURCE_FILES = [
  'src/ui.loon',
  'src/lib/utils.loon',
  'src/lib/theme.loon',
  'src/lib/components.loon',
  'src/lib/doc.loon',
  'src/router.loon',
  'src/components/footer.loon',
  'src/components/code.loon',
  'src/components/editor.loon',
  'src/components/sidebar.loon',
  'src/pages/home.loon',
  'src/pages/tour.loon',
  'src/pages/play.loon',
  'src/pages/blog.loon',
  'src/pages/roadmap.loon',
  'src/pages/install.loon',
  'src/pages/examples.loon',
  'src/pages/guide/basics.loon',
  'src/pages/guide/functions.loon',
  'src/pages/guide/types.loon',
  'src/pages/guide/collections.loon',
  'src/pages/guide/pattern-matching.loon',
  'src/pages/guide/ownership.loon',
  'src/pages/guide/effects.loon',
  'src/pages/guide/modules.loon',
  'src/pages/guide/macros.loon',
  'src/pages/guide/testing.loon',
  'src/pages/guide/errors.loon',
  'src/pages/ref/syntax.loon',
  'src/pages/ref/builtins.loon',
  'src/pages/ref/cli.loon',
  'src/pages/ref/effects.loon',
  'src/pages/ref/lsp.loon',
  'src/pages/ref/formatter.loon',
  'src/pages/concepts/invisible-types.loon',
  'src/pages/concepts/effects.loon',
  'src/pages/concepts/ownership.loon',
  'src/pages/concepts/from-rust.loon',
  'src/pages/concepts/from-js.loon',
  'src/pages/concepts/from-clojure.loon',
  'src/app.loon',
];

// --- Source cache & source map ---
const sourceCache = {};  // path -> string
let sourceMap = [];      // [{file, offset, headerLength, length}]

// Dev mode: set by SSE connection to dev server
let isDev = false;

async function loadSource(path) {
  const url = isDev ? `/${path}?t=${Date.now()}` : `/${path}`;
  const resp = await fetch(url);
  if (!resp.ok) throw new Error(`Failed to load ${path}: ${resp.status}`);
  return resp.text();
}

function buildSourceMap(sources) {
  sourceMap = [];
  let offset = 0;
  for (const path of sources) {
    const header = `; --- ${path} ---\n`;
    const content = sourceCache[path] || '';
    sourceMap.push({
      file: path,
      offset,
      headerLength: header.length,
      length: header.length + content.length,
    });
    offset += header.length + content.length + 2; // +2 for \n\n joiner
  }
}

function resolveSpan(byteOffset) {
  for (const entry of sourceMap) {
    const localStart = entry.offset + entry.headerLength;
    const localEnd = entry.offset + entry.length;
    if (byteOffset >= localStart && byteOffset < localEnd) {
      const posInFile = byteOffset - localStart;
      const content = sourceCache[entry.file] || '';
      let line = 1, col = 1;
      for (let i = 0; i < posInFile && i < content.length; i++) {
        if (content[i] === '\n') { line++; col = 1; }
        else { col++; }
      }
      return { file: entry.file, line, col };
    }
  }
  return null;
}

function concatSources() {
  return SOURCE_FILES.map(s => `; --- ${s} ---\n${sourceCache[s] || ''}`).join('\n\n');
}

// Parse a bundle back into sourceCache entries
// File headers match "; --- src/....loon ---" (contain "/" unlike internal section comments)
function parseBundleIntoCache(bundleText) {
  const headerRe = /^; --- (src\/\S+\.loon) ---$/gm;
  const headers = [];
  let m;
  while ((m = headerRe.exec(bundleText)) !== null) {
    headers.push({ path: m[1], start: m.index, contentStart: m.index + m[0].length + 1 }); // +1 for \n
  }
  for (let i = 0; i < headers.length; i++) {
    const contentStart = headers[i].contentStart;
    // Content ends at start of next file header (minus the \n\n joiner), or end of string
    const contentEnd = i + 1 < headers.length ? headers[i + 1].start - 2 : bundleText.length;
    sourceCache[headers[i].path] = bundleText.slice(contentStart, contentEnd);
  }
}

// Load sources — bundle in prod, individual files in dev
async function loadAllSources() {
  if (isDev) {
    for (const src of SOURCE_FILES) {
      sourceCache[src] = await loadSource(src);
    }
    buildSourceMap(SOURCE_FILES);
    return concatSources();
  }

  // Production: try localStorage cache first, then fetch bundle
  let fullSource;
  try {
    const cachedHash = localStorage.getItem('loon-bundle-hash');
    const currentHash = await fetch('/bundle.hash').then(r => r.text());

    const cached = localStorage.getItem('loon-bundle');
    if (cached && cachedHash === currentHash.trim()) {
      fullSource = cached;
    } else {
      fullSource = await fetch('/bundle.loon').then(r => r.text());
      try {
        localStorage.setItem('loon-bundle', fullSource);
        localStorage.setItem('loon-bundle-hash', currentHash.trim());
      } catch {
        // localStorage full — that's fine, we'll just re-fetch next time
      }
    }
  } catch {
    // bundle.hash missing — we're in dev mode, fall back to individual files
    isDev = true;
    for (const src of SOURCE_FILES) {
      sourceCache[src] = await loadSource(src);
    }
    buildSourceMap(SOURCE_FILES);
    return concatSources();
  }

  // Populate sourceCache from bundle for error resolution
  parseBundleIntoCache(fullSource);
  buildSourceMap(SOURCE_FILES);
  return fullSource;
}

// --- DOM Bridge ---
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

const listeners = new Map();
let nextListenerId = 1;

function resetDomState() {
  // Clear node table (keep handle 0 = document)
  nodes.length = 1;
  nextHandle = 1;
  // Remove all tracked event listeners
  for (const [, entry] of listeners) {
    entry.node.removeEventListener(entry.event, entry.handler);
  }
  listeners.clear();
  nextListenerId = 1;
  // Clear WASM callback registry
  try { reset_runtime(); } catch (_) {}
}

function domBridge(op, args) {
  switch (op) {
    case 'createElement': {
      const tag = args[0];
      const svgTags = ['svg','path','circle','rect','line','polyline','polygon','ellipse','g','defs','use','text','tspan','clipPath','mask','pattern','image','foreignObject','linearGradient','radialGradient','stop'];
      const el = svgTags.includes(tag)
        ? document.createElementNS('http://www.w3.org/2000/svg', tag)
        : document.createElement(tag);
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
      if (node) node.style.setProperty(args[1], args[2]);
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
      const existing = nodes.indexOf(el);
      if (existing >= 0) return existing;
      return allocHandle(el);
    }
    case 'addListener': {
      const node = getNode(args[0]);
      const event = args[1];
      const callbackId = args[2];
      if (!node) return null;
      const id = nextListenerId++;
      const handler = (e) => {
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
    case 'getValue': {
      const node = getNode(args[0]);
      return node ? (node.value || '') : '';
    }
    case 'setValue': {
      const node = getNode(args[0]);
      if (node) node.value = args[1];
      return null;
    }
    case 'evalLoon': {
      try {
        return eval_with_output(args[0]);
      } catch (e) {
        return 'Error: ' + (e.message || String(e));
      }
    }
    case 'setTitle': {
      document.title = args[0];
      return null;
    }
    case 'pushState': {
      history.pushState(null, '', args[0]);
      window.dispatchEvent(new CustomEvent('loon:navigate', { detail: { path: args[0] } }));
      return null;
    }
    case 'location': {
      return window.location.pathname;
    }
    case 'requestAnimationFrame': {
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

// --- Structured eval ---

function evalChecked(fullSource) {
  if (eval_ui_checked) {
    const result = JSON.parse(eval_ui_checked(fullSource));
    if (!result.ok) {
      const e = result.error;
      const loc = e.span ? resolveSpan(e.span[0]) : null;
      const stack = (e.stack || []).map(f => ({
        fn: f.fn,
        loc: resolveSpan(f.span[0]),
      }));
      showErrorOverlay(e.message, loc, stack);
      return false;
    }
    dismissErrorOverlay();
    return true;
  }
  // Fallback for older WASM builds
  try {
    eval_ui(fullSource);
    dismissErrorOverlay();
    return true;
  } catch (e) {
    const msg = e.message || String(e);
    showErrorOverlay(msg, null, []);
    return false;
  }
}

// --- Error Overlay ---

let overlayEl = null;

function showErrorOverlay(errorMsg, loc, stack) {
  dismissErrorOverlay();

  // Parse structured error: [E0201] message\nwhy: ...\nfix: ...
  const codeMatch = errorMsg.match(/\[([A-Z]\d{4})\]/);
  const code = codeMatch ? codeMatch[1] : null;

  const lines = errorMsg.split('\n');
  let what = lines[0];
  let why = '';
  let fix = '';

  for (const line of lines) {
    if (line.startsWith('why:')) why = line.slice(4).trim();
    else if (line.startsWith('fix:')) fix = line.slice(4).trim();
  }

  // Category from error code
  const categories = {
    'E01': 'parse error', 'E02': 'type error', 'E03': 'ownership error',
    'E04': 'effect error', 'E05': 'module error',
  };
  const category = code ? (categories[code.slice(0, 3)] || 'error') : 'runtime error';

  // Build source context snippet
  let sourceSnippet = '';
  if (loc) {
    const content = sourceCache[loc.file] || '';
    const lines = content.split('\n');
    const lineText = lines[loc.line - 1] || '';
    const pointer = ' '.repeat(loc.col - 1) + '^^^';
    sourceSnippet = `
      <div style="background:#0f0f23;border-radius:6px;padding:0.75rem 1rem;margin:0.75rem 0;overflow-x:auto">
        <p style="color:#64748b;font-size:0.75rem;margin:0 0 0.5rem">${escapeHtml(loc.file)}:${loc.line}</p>
        <pre style="color:#e2e8f0;font-size:0.8rem;margin:0;font-family:monospace;white-space:pre;line-height:1.4">${escapeHtml(lineText)}\n<span style="color:#f87171">${escapeHtml(pointer)}</span></pre>
      </div>`;
  }

  // Build stack trace
  let stackHtml = '';
  if (stack && stack.length > 0) {
    const frames = stack.map(f => {
      const locStr = f.loc ? `${f.loc.file}:${f.loc.line}` : '?';
      return `  at ${escapeHtml(f.fn)} (${escapeHtml(locStr)})`;
    }).join('\n');
    stackHtml = `
      <div style="margin-top:0.75rem">
        <pre style="color:#64748b;font-size:0.75rem;margin:0;font-family:monospace;white-space:pre;line-height:1.5">${frames}</pre>
      </div>`;
  }

  overlayEl = document.createElement('div');
  overlayEl.id = 'loon-error-overlay';
  overlayEl.innerHTML = `
    <div style="position:fixed;inset:0;background:rgba(0,0,0,0.7);backdrop-filter:blur(4px);z-index:99999;display:flex;align-items:center;justify-content:center;font-family:system-ui,-apple-system,sans-serif">
      <div style="background:#1a1a2e;border-radius:12px;max-width:40rem;width:90%;max-height:80vh;box-shadow:0 20px 60px rgba(0,0,0,0.5);overflow:hidden;display:flex;flex-direction:column">
        <div style="height:4px;background:linear-gradient(90deg,#6366f1,#8b5cf6,#a78bfa);flex-shrink:0"></div>
        <div style="padding:1.5rem 2rem;overflow-y:auto">
          <div style="display:flex;align-items:center;gap:0.75rem;margin-bottom:1rem">
            ${code ? `<span style="background:#3730a3;color:#c7d2fe;padding:2px 8px;border-radius:4px;font-size:0.8rem;font-weight:600;font-family:monospace">${code}</span>` : ''}
            <span style="color:#94a3b8;font-size:0.8rem">${category}</span>
          </div>
          <p style="color:#f1f5f9;font-size:1rem;line-height:1.5;margin:0 0 0.5rem;font-family:monospace;word-break:break-word">${escapeHtml(what)}</p>
          ${sourceSnippet}
          ${why ? `<p style="color:#94a3b8;font-size:0.85rem;line-height:1.4;margin:0.75rem 0">${escapeHtml(why)}</p>` : ''}
          ${fix ? `<div style="background:rgba(34,197,94,0.1);border:1px solid rgba(34,197,94,0.2);border-radius:6px;padding:0.75rem 1rem;margin-top:0.75rem"><p style="color:#86efac;font-size:0.85rem;margin:0"><strong>fix:</strong> ${escapeHtml(fix)}</p></div>` : ''}
          ${stackHtml}
          <p style="color:#475569;font-size:0.75rem;margin:1rem 0 0;text-align:right">press Escape to dismiss</p>
        </div>
      </div>
    </div>`;

  document.body.appendChild(overlayEl);

  const dismiss = (e) => {
    if (e.key === 'Escape') {
      dismissErrorOverlay();
      document.removeEventListener('keydown', dismiss);
    }
  };
  document.addEventListener('keydown', dismiss);
}

function dismissErrorOverlay() {
  if (overlayEl) {
    overlayEl.remove();
    overlayEl = null;
  }
}

function escapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

// --- Hot Reload ---

function connectSSE() {
  const es = new EventSource('/__dev/sse');

  es.addEventListener('loon', (e) => {
    const t0 = performance.now();
    handleLoonReload(e.data).then(() => {
      console.log(`[loon] hot reload in ${Math.round(performance.now() - t0)}ms`);
    });
  });

  es.addEventListener('css', () => {
    // Swap all stylesheets with cache-bust
    document.querySelectorAll('link[rel="stylesheet"]').forEach(link => {
      const url = new URL(link.href);
      url.searchParams.set('t', Date.now());
      link.href = url.toString();
    });
    console.log('[loon] css hot swap');
  });

  es.addEventListener('full', () => {
    window.location.reload();
  });

  es.onopen = () => console.log('[loon] dev server connected');
  es.onerror = () => {
    console.log('[loon] dev server disconnected, reconnecting...');
  };
}

async function handleLoonReload(changedFiles) {
  const files = changedFiles ? changedFiles.split(',') : SOURCE_FILES;

  // Save state
  const scrollX = window.scrollX;
  const scrollY = window.scrollY;
  const path = window.location.pathname;
  const editorEl = document.querySelector('.playground-editor textarea');
  const editorContent = editorEl ? editorEl.value : null;

  // Re-fetch changed files
  for (const file of files) {
    const trimmed = file.trim();
    if (sourceCache[trimmed] !== undefined) {
      try {
        sourceCache[trimmed] = await loadSource(trimmed);
      } catch (e) {
        console.warn(`[loon] failed to reload ${trimmed}:`, e);
      }
    }
  }

  // Rebuild and re-eval
  buildSourceMap(SOURCE_FILES);
  const fullSource = concatSources();

  // Clear app content
  const app = document.getElementById('app');
  if (app) app.innerHTML = '';

  resetDomState();
  init_dom_bridge(domBridge);
  window.__loon_event_handler = (callbackId) => invoke_callback(callbackId);
  window.__loon_timeout_handler = (callbackId) => invoke_callback(callbackId);

  if (!evalChecked(fullSource)) {
    console.error('[loon] reload error');
  }

  // Restore state
  requestAnimationFrame(() => {
    window.scrollTo(scrollX, scrollY);
    if (editorContent && path === '/play') {
      const el = document.querySelector('.playground-editor textarea');
      if (el) el.value = editorContent;
    }
  });
}

// --- Boot ---

async function boot() {
  try {
    performance.mark('boot-start');

    // Parallel: init WASM + load sources
    // loadAllSources() detects dev vs prod by checking for bundle.hash
    const [_, fullSource] = await Promise.all([
      init().then(() => wasmReady),
      loadAllSources(),
    ]);
    performance.mark('sources-loaded');

    init_dom_bridge(domBridge);

    window.__loon_event_handler = (callbackId) => {
      invoke_callback(callbackId);
    };
    window.__loon_timeout_handler = (callbackId) => invoke_callback(callbackId);

    performance.mark('eval-start');
    evalChecked(fullSource);
    performance.mark('eval-done');

    performance.mark('boot-done');

    // Log performance
    performance.measure('wasm+fetch', 'boot-start', 'sources-loaded');
    performance.measure('eval', 'eval-start', 'eval-done');
    performance.measure('total-boot', 'boot-start', 'boot-done');

    const measures = performance.getEntriesByType('measure');
    const fmt = m => `${m.name}: ${Math.round(m.duration)}ms`;
    console.log(`[loon] boot ${measures.map(fmt).join(', ')}`);

    // Connect to dev server SSE for hot reload
    if (isDev) connectSSE();
  } catch (e) {
    const msg = e.message || String(e);
    console.error('Loon boot failed:', msg);
    showErrorOverlay(msg, null, []);
  }
}

window.__loon_eval = eval_program;
window.__loon_enable_effect_log = (v) => enable_effect_log && enable_effect_log(v);
window.__loon_get_effect_log = () => get_effect_log ? JSON.parse(get_effect_log()) : [];
window.__loon_clear_effect_log = () => clear_effect_log && clear_effect_log();

// Click-to-copy for .copyable code blocks
document.addEventListener('click', (e) => {
  const el = e.target.closest('.copyable');
  if (!el) return;
  const text = el.textContent.trim();
  navigator.clipboard.writeText(text).then(() => {
    el.classList.add('copied');
    setTimeout(() => el.classList.remove('copied'), 1500);
  });
});

boot();
