// Bundle Melange's ES module output into dist/: the static site (index.html, app.js, style.css,
// sw.js, sim-progressor.js). Run after `dune build @web` (npm run build does both).
import { build } from "esbuild";
import { cpSync, mkdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { gzipSync } from "node:zlib";
import { stampShell } from "./shell.mjs";

const root = fileURLToPath(new URL("..", import.meta.url));
const dist = `${root}dist`;

rmSync(dist, { recursive: true, force: true });
mkdirSync(dist, { recursive: true });

await build({
  // dune writes the melange.emit `js` target here; bare imports (react, melange.js/…) resolve
  // through _build/default/web/src/js/node_modules (Melange runtime) and node_modules (React).
  entryPoints: { app: `${root}_build/default/web/src/js/web/src/main.js` },
  bundle: true,
  format: "esm",
  target: ["safari16", "chrome115"], // Bluefy uses iOS WebKit
  minify: true,
  sourcemap: "linked",
  define: { "process.env.NODE_ENV": '"production"' },
  outdir: dist,
  logLevel: "warning",
});

cpSync(`${root}web/static`, dist, { recursive: true });
// GitHub Pages: serve the files as they are (no Jekyll).
writeFileSync(`${dist}/.nojekyll`, "");

// index.html pointing at this build's bundle and stylesheet (?v=<content hash>), and the service
// worker that keeps them in the browser.
const shell = stampShell(dist);

const js = `${dist}/app.js`;
const kb = (n) => (n / 1024).toFixed(1) + " KiB";
console.log(`dist/app.js (build ${shell.build}): ${kb(statSync(js).size)} (${kb(gzipSync(readFileSync(js)).length)} gzip)`);
