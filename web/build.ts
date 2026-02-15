import { execSync } from 'child_process';
import { cpSync, mkdirSync, readFileSync, writeFileSync, readdirSync, statSync } from 'fs';
import { join, relative } from 'path';

const ROOT = join(import.meta.dirname, '..');
const WEB = import.meta.dirname;
const DIST = join(WEB, 'dist');
const PUBLIC = join(WEB, 'public');
const SRC = join(WEB, 'src');

const isDev = process.argv.includes('--dev');

// Step 1: Build WASM
console.log('Building loon-wasm...');
execSync(
  `wasm-pack build ${join(ROOT, 'crates/loon-wasm')} --target web --out-dir ${join(DIST)}`,
  { stdio: 'inherit' }
);

// Step 2: Copy static assets
console.log('Copying static assets...');
mkdirSync(DIST, { recursive: true });
for (const file of ['index.html', 'boot.js', 'style.css', 'loon-bird.svg']) {
  try {
    cpSync(join(PUBLIC, file), join(DIST, file));
  } catch {
    // File might not exist yet (e.g., loon-bird.svg)
  }
}

// Step 3: Copy Loon source files to dist/src/
function copyDir(src: string, dest: string) {
  mkdirSync(dest, { recursive: true });
  for (const entry of readdirSync(src)) {
    const srcPath = join(src, entry);
    const destPath = join(dest, entry);
    if (statSync(srcPath).isDirectory()) {
      copyDir(srcPath, destPath);
    } else {
      cpSync(srcPath, destPath);
    }
  }
}

console.log('Copying Loon source files...');
copyDir(SRC, join(DIST, 'src'));

// Step 4: Generate pre-rendered HTML (if not dev mode)
if (!isDev) {
  console.log('Pre-rendering pages...');
  // Run native interpreter to generate static HTML for each page
  // This gives us SEO-friendly content that hydrates when WASM loads
  try {
    // TODO: Implement pre-rendering with native interpreter
    console.log('  (pre-rendering not yet implemented, shipping SPA-only)');
  } catch (e) {
    console.warn('Pre-rendering failed, continuing with SPA-only:', e);
  }
}

console.log(`Build complete! Output in ${relative(process.cwd(), DIST)}`);
