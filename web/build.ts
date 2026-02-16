import { execSync } from 'child_process';
import { cpSync, mkdirSync, readFileSync, writeFileSync, readdirSync, statSync, unlinkSync } from 'fs';
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

// Step 4: Type-check Loon sources
console.log('Type-checking Loon sources...');
{
  const bootOrder = [
    'src/ui.loon',
    'src/lib/utils.loon',
    'src/lib/theme.loon',
    'src/lib/components.loon',
    'src/router.loon',
    'src/components/footer.loon',
    'src/components/code.loon',
    'src/components/editor.loon',
    'src/pages/home.loon',
    'src/pages/tour.loon',
    'src/pages/play.loon',
    'src/pages/blog.loon',
    'src/app.loon',
  ];

  const parts: string[] = [];
  for (const file of bootOrder) {
    const fullPath = join(SRC, '..', file);
    try {
      parts.push(readFileSync(fullPath, 'utf-8'));
    } catch {
      // File may not exist (e.g., deleted pages)
    }
  }

  const tmpFile = join(DIST, '_check.loon');
  writeFileSync(tmpFile, parts.join('\n\n'));

  try {
    execSync(
      `cargo run -p loon-cli --quiet -- check ${tmpFile}`,
      { stdio: 'inherit', cwd: ROOT }
    );
  } catch {
    console.error('Type-check failed! Fix errors above before deploying.');
    process.exit(1);
  }

  // Clean up temp file
  try { unlinkSync(tmpFile); } catch {}
}

// Step 5: Generate pre-rendered HTML (if not dev mode)
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
