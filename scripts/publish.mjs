// Publish dist/ to the `gh-pages` branch of `origin`, which GitHub Pages serves.
// The branch holds only the built site, as a single commit that each publish replaces.
// Run through `npm run publish` (which builds first).
import { execFileSync } from "node:child_process";
import { existsSync, rmSync } from "node:fs";
import { fileURLToPath } from "node:url";

const root = fileURLToPath(new URL("..", import.meta.url));
const dist = `${root}dist`;
if (!existsSync(`${dist}/index.html`)) throw new Error("dist/ is missing: run npm run build");

const git = (cwd, ...args) => execFileSync("git", args, { cwd, encoding: "utf8" }).trim();
const remote = git(root, "remote", "get-url", "origin");
const source = git(root, "rev-parse", "--short", "HEAD");
const dirty = git(root, "status", "--porcelain") !== "";

rmSync(`${dist}/.git`, { recursive: true, force: true });
try {
  git(dist, "init", "--quiet", "--initial-branch=gh-pages");
  git(dist, "add", "--all");
  git(dist, "-c", `user.name=${git(root, "config", "user.name")}`, "-c", `user.email=${git(root, "config", "user.email")}`,
    "commit", "--quiet", "-m", `Site built from ${source}${dirty ? " (with uncommitted changes)" : ""}`);
  git(dist, "push", "--force", "--quiet", remote, "gh-pages");
} finally {
  rmSync(`${dist}/.git`, { recursive: true, force: true });
}
console.log(`Published ${source}${dirty ? " (with uncommitted changes)" : ""} to gh-pages of ${remote}`);
