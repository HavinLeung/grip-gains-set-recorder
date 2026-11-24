// Serve dist/ on http://localhost:8080/grip-gains-set-recorder/ : under a sub-path, the way
// GitHub Pages serves it, so a path that is not relative shows up here. PORT changes the port.
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, normalize } from "node:path";
import { fileURLToPath } from "node:url";

const dist = fileURLToPath(new URL("../dist", import.meta.url));
const base = "/grip-gains-set-recorder/";
const port = Number(process.env.PORT ?? 8080);
const types = { ".html": "text/html; charset=utf-8", ".js": "text/javascript; charset=utf-8", ".css": "text/css; charset=utf-8", ".map": "application/json" };

createServer(async (req, res) => {
  const { pathname } = new URL(req.url, "http://localhost");
  if (pathname === base.slice(0, -1)) return res.writeHead(301, { Location: base }).end();
  if (!pathname.startsWith(base)) return res.writeHead(404).end("Not found");
  const rel = normalize(pathname.slice(base.length) || "index.html");
  if (rel.startsWith("..")) return res.writeHead(404).end("Not found");
  try {
    const body = await readFile(`${dist}/${rel.endsWith("/") ? rel + "index.html" : rel}`);
    res.writeHead(200, { "Content-Type": types[extname(rel)] ?? "application/octet-stream", "Cache-Control": "no-cache" }).end(body);
  } catch {
    res.writeHead(404).end("Not found");
  }
}).listen(port, "127.0.0.1", () => console.log(`http://localhost:${port}${base}  (simulated Progressor: add ?sim=1)`));
