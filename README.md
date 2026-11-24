# Grip Gains Set Recorder

A set timer for grip training with a [Tindeq Progressor](https://tindeq.com),
running entirely in the browser. It connects to the Progressor over Web Bluetooth, shows the force
live against a target weight, times each rep and rest, and keeps a history of your sets with a
force graph for every rep.

There is no server and no account. The page is a handful of static files; your sets and settings
stay in the browser you record them in.

## What you need

- A **Tindeq Progressor**.
- A browser with Web Bluetooth:
  - **iPhone / iPad**: the Bluefy browser from the App Store (Safari has no Web Bluetooth). Open
    the page's address directly in Bluefy.
  - **Android, Mac, Windows, Linux**: Chrome or Edge.
- No Progressor at hand? Add `?sim=1` to the address for a simulated one (a small panel at the
  bottom left "grips" with a slider or a timed hold), or switch on **Manual** and time reps by
  tapping.

## Using it

1. **Connect Progressor** and pick it in the browser's Bluetooth list. If another app is connected
   to it (the Tindeq app, for one), close that app first: the Progressor takes one connection.
2. Pick the gripper, the hand, the target weight (lbs) and the number of reps. Hanging the load on
   the Progressor outside a set weighs it; **Use as target** takes that weight.
3. **Start set.** After the countdown each rep starts at GO, whether or not you are gripping yet.
   It ends when the force drops below the threshold after having reached the target zone, or when
   you press **End rep**. The rest then counts down and starts the next rep.
4. When the set ends, correct the logged weight if needed and **Save** (or discard).

**History** lists saved sets; open one for its rep graphs and statistics, copy it as JSON, or
delete it. **Settings** has the countdown and rest times, the thresholds, sounds, page size,
screen wake lock and tare. The settings that shape a set are locked while one is in progress.

**Manual mode** needs no Progressor: there is no force graph, and every rep ends when you tap
**End rep**.

**PowerCurve** (optional): paste your upload token from PowerCurve's Grip Gains import helper in
Settings to upload sets to [PowerCurve](https://powercurve.tantaluspath.com), either one by one
from History or automatically when a set is saved. The token is stored in this browser only and is
sent only to PowerCurve.

## Where your data is

Everything stays in the browser, per site address:

| What | Where |
|---|---|
| Saved sets, with every rep's 20 Hz force trace | IndexedDB database `grip-gains-set-recorder-v1` |
| Settings | localStorage `grip-gains-set-recorder.settings.v1` |
| PowerCurve token, if you set one | localStorage `grip-gains-set-recorder.powercurve-token.v1` |
| The app itself, so it opens offline | the service worker's cache `ggsr-shell-<build>` |

Nothing is sent anywhere, except a set you upload to PowerCurve. That also means there is no
backup and no sync between devices: clearing the browser's site data deletes your history
(**Copy JSON** in History exports sets). Weights are shown in pounds.

## Building

The app is OCaml: [ReasonReact](https://reasonml.github.io/reason-react/) components written in
[mlx](https://github.com/ocaml-mlx/mlx) (OCaml with JSX), compiled to JavaScript by
[Melange](https://melange.re) and bundled with esbuild.

You need [opam](https://opam.ocaml.org) 2.x and a recent Node. One-time setup, in the checkout:

```sh
opam switch create . ocaml-base-compiler.5.5.2 --no-install   # a local switch in ./_opam
opam install . --deps-only --with-dev-setup -y                 # versions are pinned in dune-project
npm install
```

(To reuse a switch you already have with these packages, set `OPAMSWITCH` to it instead of
creating one.)

| | |
|---|---|
| `npm run build` | `dune build @web`, then esbuild: the static site in `dist/` |
| `npm run serve` | serves `dist/` at http://localhost:8080/grip-gains-set-recorder/ (add `?sim=1`). `localhost` is a secure context, so a real Progressor works from desktop Chrome too |
| `npm test` | the fast checks, no browser: `dune test` (the timer logic's unit tests) and the web layer's pure checks in Node |
| `npm run publish` | builds, then force-pushes `dist/` as the single commit of the `gh-pages` branch of `origin`, which GitHub Pages serves |
| `opam exec -- dune fmt` | formats the OCaml and mlx sources |

Every path in the built site is relative, so it can be served from any directory of any static
host over HTTPS. To host your own copy on GitHub Pages: fork, `npm run publish`, and set
Settings → Pages to deploy from the `gh-pages` branch.

## Layout

```
timer_core/        the timer's logic, pure OCaml with no browser in it: Session.step takes an
                   input (a Bluetooth packet, a 100 ms tick, a button) and returns the new state
                   and events. Also the Progressor's protocol, rep statistics, settings and
                   history formats. Built natively for its tests and by Melange for the app.
  test/            its unit tests. ("The MVP" in comments is the app's first version, a single
                   HTML page in JavaScript, which this library was ported from.)
web/src/           the app
  main.mlx         entry point
  timer_engine     the controller around Session: Bluetooth, the tick, the frame loop, sound,
                   wake lock, the end-of-set review, PowerCurve
  timer_tab, timer_history, timer_settings, timer_parts (.mlx)   the views
  timer_sets, timer_db, timer_summary                            saved sets (IndexedDB)
  timer_draw, timer_audio, timer_log, timer_ui                   canvas, WebAudio, log, formatting
  app_prefs, timer_prefs, grippers, set_types                    settings and fixed lists
  browser, idb, http, decode                                     browser bindings
  timer_checks.ml  pure checks run in Node (not in the bundle)
web/static/        index.html, style.css, sw.js (service worker), sim-progressor.js (?sim=1)
scripts/           bundle-web.mjs + shell.mjs (build), serve.mjs, publish.mjs
```

A few things worth knowing before changing the timer:

- Rep times come from the Progressor's own sample clock, not the page's, so a stalled browser
  frame or a late Bluetooth packet doesn't change a rep's length. A restart of the device's
  counter mid-set is handled.
- Weighing a load outside a set uses the median of the last 2 seconds.
- The core works in kilograms (the Progressor's unit); the web layer shows pounds.
- `Printf`, `Hashtbl` and `Map.Make` are avoided in code that reaches the bundle: they add tens
  of kilobytes.

## Licence

[MIT](LICENSE).
