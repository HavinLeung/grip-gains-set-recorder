// The app shell for offline use: stamps index.html's references to the bundle and the stylesheet
// with a version taken from their content ("app.js?v=<hash>"), and writes the service worker
// (web/static/sw.js) with the list of files to keep in the browser and their SHA-256.
// Called by bundle-web.mjs. Every URL is relative, so the build can be served from any sub-path.
//
// `dir` must already hold esbuild's app.js and a fresh copy of web/static.
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";

const sha256 = (data) => createHash("sha256").update(data).digest("hex");

export function stampShell(dir) {
  const asset = (name) => {
    const hash = sha256(readFileSync(`${dir}/${name}`));
    return { url: `${name}?v=${hash.slice(0, 12)}`, sha256: hash };
  };
  const app = asset("app.js");
  const style = asset("style.css");

  const replaceOnce = (text, from, to) => {
    if (text.split(from).length !== 2) throw new Error(`expected exactly one ${from}`);
    return text.replace(from, to);
  };
  let html = readFileSync(`${dir}/index.html`, "utf8");
  html = replaceOnce(html, 'src="app.js"', `src="${app.url}"`);
  html = replaceOnce(html, 'href="style.css"', `href="${style.url}"`);
  writeFileSync(`${dir}/index.html`, html);

  let sw = readFileSync(`${dir}/sw.js`, "utf8");
  // The build id changes whenever anything the service worker keeps (or the worker itself) changes.
  const build = sha256([html, app.sha256, style.sha256, readFileSync(`${dir}/sim-progressor.js`), sw].join("\n")).slice(0, 12);
  const shell = { build, page: "./", assets: [app, style], plain: ["sim-progressor.js"] };
  sw = replaceOnce(sw, "/*__SHELL__*/ null", JSON.stringify(shell));
  writeFileSync(`${dir}/sw.js`, sw);
  return shell;
}
