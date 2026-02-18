import { createHash } from 'crypto';
import { execSync } from 'child_process';
import { cpSync, mkdirSync, readFileSync, writeFileSync, readdirSync, statSync, unlinkSync } from 'fs';
import { join, relative } from 'path';

const ROOT = join(import.meta.dirname, '..');
const WEB = import.meta.dirname;
const DIST = join(WEB, 'dist');
const PUBLIC = join(WEB, 'public');
const SRC = join(WEB, 'src');

const isDev = process.argv.includes('--dev');
const noRust = process.argv.includes('--no-rust') || process.env.VERCEL === '1';

const bootOrder = [
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

// Step 1: Build WASM (or copy pre-built artifacts)
if (noRust) {
  console.log('Copying pre-built WASM artifacts...');
  mkdirSync(DIST, { recursive: true });
  for (const file of readdirSync(join(PUBLIC, 'pkg'))) {
    if (file === 'package.json') continue;
    cpSync(join(PUBLIC, 'pkg', file), join(DIST, file));
  }
} else {
  console.log('Building loon-wasm...');
  execSync(
    `wasm-pack build ${join(ROOT, 'crates/loon-wasm')} --target web --out-dir ${join(DIST)}`,
    { stdio: 'inherit' }
  );
}

// Step 2: Copy static assets
console.log('Copying static assets...');
mkdirSync(DIST, { recursive: true });
for (const file of ['index.html', 'boot.js', 'style.css']) {
  try {
    cpSync(join(PUBLIC, file), join(DIST, file));
  } catch {
    // File might not exist yet (e.g., loon-bird.svg)
  }
}

// Copy fonts
try {
  copyDir(join(PUBLIC, 'fonts'), join(DIST, 'fonts'));
} catch {
  // fonts dir might not exist
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

// Step 4: Bundle .loon sources into a single file
console.log('Bundling Loon sources...');
const bundleParts: string[] = [];
for (const file of bootOrder) {
  const fullPath = join(SRC, '..', file);
  try {
    bundleParts.push(`; --- ${file} ---\n${readFileSync(fullPath, 'utf-8')}`);
  } catch {
    // File may not exist
  }
}
const bundle = bundleParts.join('\n\n');
writeFileSync(join(DIST, 'bundle.loon'), bundle);
const bundleHash = createHash('md5').update(bundle).digest('hex').slice(0, 8);
writeFileSync(join(DIST, 'bundle.hash'), bundleHash);
console.log(`  bundle.loon (${(bundle.length / 1024).toFixed(1)}KB, hash: ${bundleHash})`);

// Step 5: Type-check Loon sources (skip without Rust toolchain)
// (reuses bootOrder from top-level constant)
if (!noRust) {
  console.log('Type-checking Loon sources...');
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
} else {
  console.log('Skipping type-check (--no-rust)...');
}

// Step 6: Generate pre-rendered HTML (if not dev mode)
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
