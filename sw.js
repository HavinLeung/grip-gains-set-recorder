// The service worker: keeps the app (the page, the script bundle, the stylesheet, the simulator)
// in this browser so it opens with no connection. Hand-written, no dependencies.
//
// Every path is relative to this file, so the app works from any sub-path (GitHub Pages serves
// it from /<repository>/).
//
// - One cache per build ("ggsr-shell-<build>"), filled at install from the list the build wrote
//   below. Everything is fetched fresh and must be a plain 200, or the install fails and the
//   previous worker (and its cache) stays.
// - The page: network first, so an online visit always gets the newest build. The cached page is
//   used only when the network fails or does not answer within a few seconds.
// - The bundle and stylesheet are named with a version from their content ("app.js?v=<hash>"),
//   and the page names the exact ones it was built with. Exactly those URLs are served from the
//   cache; any other version goes to the network. At install their bytes are checked against the
//   build's SHA-256, so what is kept is always one build's page, script and stylesheet together.
// - The simulator (sim-progressor.js, loaded only for ?sim=1) is network first too.
// - Nothing else is touched, and nothing is ever added to the cache after install.
const SHELL = {"build":"066bc8e1f9ae","page":"./","assets":[{"url":"app.js?v=4817ce0a2e6c","sha256":"4817ce0a2e6c59243049c2563b28e9762e6f853e69b4fd9605b6faf043be4a32"},{"url":"style.css?v=3ae731e667b4","sha256":"3ae731e667b48749d8745aab74df607fae0e77eeb1074099f4f0cb66b9fe55a6"}],"plain":["sim-progressor.js"]}; // { build, page, assets: [{ url, sha256 }], plain: [...] }, written by scripts/shell.mjs

const PREFIX = "ggsr-shell-";
const CACHE = PREFIX + SHELL.build;
const PAGE_TIMEOUT_MS = 4000;

// "app.js?v=1234" -> "/<base>/app.js?v=1234": what the cache is keyed by and requests are matched on.
const absolute = (relative) => {
  const url = new URL(relative, self.location);
  return url.pathname + url.search;
};
const PAGE = absolute(SHELL.page);
const ASSETS = SHELL.assets.map((a) => ({ path: absolute(a.url), sha256: a.sha256 }));
const PLAIN = SHELL.plain.map(absolute);

async function fetchForCache(path) {
  const res = await fetch(new Request(path, { cache: "reload", credentials: "same-origin", redirect: "error" }));
  if (res.status !== 200 || res.redirected || res.type !== "basic") throw new Error(`${path}: ${res.status}`);
  return res;
}

const hex = (buffer) => [...new Uint8Array(buffer)].map((b) => b.toString(16).padStart(2, "0")).join("");

async function install() {
  const paths = [PAGE, ...ASSETS.map((a) => a.path), ...PLAIN];
  const responses = await Promise.all(paths.map(fetchForCache));
  // Everything must be this build's (a deploy may land between our requests): the page names
  // this build's bundle and stylesheet, and their bytes match the build's hashes.
  const html = await responses[0].clone().text();
  if (!SHELL.assets.every((a) => html.includes(`"${a.url}"`))) throw new Error("page is from another build");
  for (const [i, a] of ASSETS.entries()) {
    const digest = await crypto.subtle.digest("SHA-256", await responses[i + 1].clone().arrayBuffer());
    if (hex(digest) !== a.sha256) throw new Error(`${a.path} is from another build`);
  }
  const cache = await caches.open(CACHE);
  await Promise.all(paths.map((path, i) => cache.put(path, responses[i])));
}

async function activate() {
  for (const name of await caches.keys()) if (name.startsWith(PREFIX) && name !== CACHE) await caches.delete(name);
  await self.clients.claim();
}

const cached = async (path) => (await caches.open(CACHE)).match(path);

// Network first; the cached copy if the network fails, or meanwhile if it is slow.
async function networkFirst(request, path, timeoutMs) {
  const network = fetch(request);
  const fallback = async () => (await cached(path)) ?? network;
  let timer;
  const slow = new Promise((resolve) => { timer = setTimeout(() => resolve(null), timeoutMs); });
  try {
    const res = await Promise.race([network, slow]);
    return res ?? (await fallback());
  } catch {
    return (await cached(path)) ?? Response.error();
  } finally {
    clearTimeout(timer);
    network.catch(() => {});
  }
}

async function cacheFirst(request, path) {
  return (await cached(path)) ?? fetch(request);
}

self.addEventListener("install", (event) => {
  // Take over as soon as the new cache is complete: the page is network-first and the assets are
  // versioned by content, so an open page is not disturbed.
  event.waitUntil(install().then(() => self.skipWaiting()));
});
self.addEventListener("activate", (event) => event.waitUntil(activate()));
self.addEventListener("fetch", (event) => {
  const request = event.request;
  if (request.method !== "GET") return;
  const url = new URL(request.url);
  if (url.origin !== self.location.origin) return;
  const path = url.pathname + url.search;
  if (request.mode === "navigate") {
    // The page, with any query (?sim=1), also when asked for as index.html.
    if (url.pathname === PAGE || url.pathname === PAGE + "index.html") event.respondWith(networkFirst(request, PAGE, PAGE_TIMEOUT_MS));
  } else if (ASSETS.some((a) => a.path === path)) {
    event.respondWith(cacheFirst(request, path));
  } else if (PLAIN.includes(url.pathname)) {
    event.respondWith(networkFirst(request, url.pathname, PAGE_TIMEOUT_MS));
  }
});
