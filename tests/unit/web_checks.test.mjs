// The web app's pure checks, run in Node on the Melange build (`dune build @web`), with no
// browser: part of `npm test`.
import { test } from "node:test";
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";

const js = fileURLToPath(new URL("../../_build/default/web/src/js/web/src/", import.meta.url));

test("Timer: set types, a set's summary, Copy JSON, manual sets (timer_checks.ml)", async () => {
  const m = await import(`${js}timer_checks.js`);
  assert.deepEqual([...m.run()], []);
});
