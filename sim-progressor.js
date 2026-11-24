// A simulated Tindeq Progressor, for trying the app without the device: it replaces
// navigator.bluetooth with the subset the Timer uses (requestDevice -> GATT -> the Progressor
// service's notify and write characteristics) and speaks the real protocol:
//
//   commands (one byte written): 0x6F battery, 0x65 start measuring, 0x64 tare, 0x6E shut down
//   battery reply:  [0x00, 0x04, uint32 LE millivolts]
//   weight packets: [0x01, n*8, (float32 LE kg, uint32 LE device microseconds) x n]
//                   16 samples (80 Hz) every ~200 ms, device time strictly 12.5 ms apart
//
// The force follows a script: window.ggSim.hold(kg, seconds), setForce(kg), or the slider on the
// small panel this file adds to the page. Disconnects (radio drop, power off) fire
// "gattserverdisconnected" like a real device.
//
// The app loads this file only when the page is opened with ?sim=1.
(() => {
  "use strict";
  if (window.ggSim) return;

  const SVC = "7e4e1701-1ea6-40c9-9dcc-13d34ffead57";
  const NOTIFY = "7e4e1702-1ea6-40c9-9dcc-13d34ffead57";
  const WRITE = "7e4e1703-1ea6-40c9-9dcc-13d34ffead57";
  const wait = (ms) => new Promise((r) => setTimeout(r, ms));
  const err = (name, message) => Object.assign(new Error(message), { name });

  const sim = {
    millivolts: 3950, // ~74% on the Li curve
    noiseKg: 0.04,
    rampMs: 120,
    powered: true,
    // Behaviour switches for tests
    cancelNextPick: false,
    failConnects: 0, // the next N gatt.connect() calls fail
    unreachableUntil: 0, // connect() fails until this performance.now()
    silentCommands: false, // writes succeed but the device ignores them (a stuck device)
    failStarts: 0, // the next N "start measuring" writes fail (the link is up, the command is refused)
    // Script: force(t) for page time t (ms)
    level: 0, // the slider / setForce level (kg)
    holds: [], // [{start, end, kg}]
    raw: 0,
    tareOffset: 0,
    counterUs: Math.floor(Math.random() * 2 ** 31),
    streaming: false,
    packets: 0,
    commands: [],
    device: null,
  };

  // Smoothly approach a level (first-order lag), plus noise.
  let shownKg = 0;
  function targetAt(t) {
    for (const h of sim.holds) if (t >= h.start && t < h.end) return h.kg;
    return sim.level;
  }
  function sample(t, dtMs) {
    const goal = targetAt(t);
    const a = 1 - Math.exp(-dtMs / Math.max(1, sim.rampMs / 3));
    shownKg += (goal - shownKg) * a;
    const noise = (Math.random() + Math.random() + Math.random() - 1.5) * sim.noiseKg * 2;
    sim.raw = shownKg + (shownKg > 0.5 ? noise : noise / 4);
    return sim.raw - sim.tareOffset;
  }

  class Characteristic extends EventTarget {
    constructor(service, uuid) { super(); this.service = service; this.uuid = uuid; this.value = null; this.notifying = false; }
    async startNotifications() { await wait(20); this.requireConnected(); this.notifying = true; return this; }
    async stopNotifications() { await wait(5); this.notifying = false; return this; }
    requireConnected() { if (!sim.device || !sim.device.gatt.connected) throw err("NetworkError", "GATT Server is disconnected. Cannot perform GATT operations."); }
    async writeValueWithResponse(data) {
      await wait(15);
      this.requireConnected();
      const bytes = new Uint8Array(data.buffer ? data.buffer : data);
      if (bytes[0] === 0x65 && sim.failStarts > 0) { sim.failStarts--; throw err("NetworkError", "GATT operation failed for unknown reason."); }
      for (const b of bytes) command(b);
    }
    async writeValue(data) { return this.writeValueWithResponse(data); }
    notify(bytes) {
      if (!this.notifying) return;
      this.value = new DataView(bytes.buffer);
      this.dispatchEvent(new Event("characteristicvaluechanged"));
    }
  }
  class Service {
    constructor() { this.uuid = SVC; this.notifyChar = new Characteristic(this, NOTIFY); this.writeChar = new Characteristic(this, WRITE); }
    async getCharacteristic(uuid) {
      await wait(10);
      if (uuid === NOTIFY) return this.notifyChar;
      if (uuid === WRITE) return this.writeChar;
      throw err("NotFoundError", "No Characteristics matching UUID " + uuid + " found in Service.");
    }
  }
  class Gatt {
    constructor(device) { this.device = device; this.connected = false; this.service = new Service(); }
    async connect() {
      await wait(150);
      if (!sim.powered) { await wait(2000); throw err("NetworkError", "Connection attempt failed."); }
      if (sim.failConnects > 0) { sim.failConnects--; throw err("NetworkError", "Connection failed for unknown reason."); }
      if (performance.now() < sim.unreachableUntil) throw err("NetworkError", "Connection failed for unknown reason.");
      this.connected = true;
      return this;
    }
    disconnect() {
      if (!this.connected) return;
      this.connected = false;
      stopStream();
      this.service.notifyChar.notifying = false;
      // Chrome fires gattserverdisconnected for a disconnect() made by the page too.
      setTimeout(() => this.device.dispatchEvent(new Event("gattserverdisconnected")), 0);
    }
    async getPrimaryService(uuid) {
      await wait(30);
      if (!this.connected) throw err("NetworkError", "GATT Server is disconnected.");
      if (uuid !== SVC) throw err("NotFoundError", "No Services matching UUID " + uuid + " found in Device.");
      return this.service;
    }
  }
  class Device extends EventTarget {
    constructor() { super(); this.id = "sim-" + Math.random().toString(36).slice(2); this.name = "Progressor_SIM"; this.gatt = new Gatt(this); }
  }

  // --- the device's behaviour ---
  function command(b) {
    sim.commands.push(b);
    if (sim.silentCommands) return;
    const ch = sim.device.gatt.service.notifyChar;
    if (b === 0x6f) { // battery
      setTimeout(() => {
        const a = new Uint8Array(6), v = new DataView(a.buffer);
        a[0] = 0x00; a[1] = 0x04; v.setUint32(2, sim.millivolts, true);
        ch.notify(a);
      }, 40);
    } else if (b === 0x65) startStream();
    else if (b === 0x64) { sim.tareOffset = sim.raw; sim.counterUs = 0; } // tare zeroes and restarts the counter
    else if (b === 0x6e) powerOff();
  }
  let timer = null, lastEmit = 0;
  function startStream() {
    if (sim.streaming) return;
    sim.streaming = true;
    lastEmit = performance.now();
    const tick = () => {
      if (!sim.streaming) return;
      const now = performance.now();
      const n = 16, a = new Uint8Array(2 + n * 8), v = new DataView(a.buffer);
      a[0] = 0x01; a[1] = n * 8;
      for (let i = 0; i < n; i++) {
        const t = lastEmit + (i + 1) * 12.5;
        v.setFloat32(2 + i * 8, sample(t, 12.5), true);
        v.setUint32(6 + i * 8, sim.counterUs >>> 0, true);
        sim.counterUs = (sim.counterUs + 12500) % 4294967296;
      }
      lastEmit += n * 12.5;
      if (now - lastEmit > 1000) lastEmit = now; // the page was asleep: don't replay the gap
      sim.packets++;
      sim.device.gatt.service.notifyChar.notify(a);
      // Next batch when its last sample is due (+ up to 30 ms of BLE jitter), so the simulated clock keeps up
      // with real time like a device's. A fixed 185-215 ms timeout let it fall behind whenever
      // a timer fired late or the page was busy (a heavy tab rendering), and that lag (up to the
      // 1 s reset above) showed up as longer rep times.
      timer = setTimeout(tick, Math.max(0, lastEmit + n * 12.5 - performance.now() + Math.random() * 30));
    };
    timer = setTimeout(tick, 200);
  }
  function stopStream() { sim.streaming = false; clearTimeout(timer); }
  function powerOff() {
    sim.powered = false;
    stopStream();
    const g = sim.device && sim.device.gatt;
    if (g && g.connected) { g.connected = false; setTimeout(() => sim.device.dispatchEvent(new Event("gattserverdisconnected")), 100); }
  }

  // --- the navigator.bluetooth surface ---
  const bluetooth = {
    async getAvailability() { return true; },
    async requestDevice(opts) {
      await wait(250);
      if (sim.cancelNextPick) { sim.cancelNextPick = false; throw err("NotFoundError", "User cancelled the requestDevice() chooser."); }
      const ok = opts && (opts.acceptAllDevices || (opts.filters || []).some((f) => (f.namePrefix && "Progressor_SIM".startsWith(f.namePrefix)) || (f.services || []).includes(SVC)));
      if (!ok) throw err("NotFoundError", "No device matches the filters.");
      if (!sim.device) sim.device = new Device();
      return sim.device;
    },
  };
  Object.defineProperty(navigator, "bluetooth", { value: bluetooth, configurable: true });

  // --- the control API (tests and the panel) ---
  Object.assign(sim, {
    setForce(kg) { sim.level = Math.max(0, kg); sim.holds = []; renderPanel(); },
    // Hold kg for seconds, starting after delayMs; resolves when released.
    hold(kg, seconds, delayMs = 0) {
      const start = performance.now() + delayMs, end = start + seconds * 1000;
      sim.holds.push({ start, end, kg });
      return wait(delayMs + seconds * 1000 + sim.rampMs * 2);
    },
    release() { sim.holds = []; sim.level = 0; renderPanel(); },
    // A radio drop: the link goes down now; reconnects fail for ms milliseconds.
    drop(ms = 0) {
      sim.unreachableUntil = performance.now() + ms;
      const g = sim.device && sim.device.gatt;
      if (g && g.connected) { g.connected = false; stopStream(); g.service.notifyChar.notifying = false; g.device.dispatchEvent(new Event("gattserverdisconnected")); }
    },
    powerOn() { sim.powered = true; renderPanel(); },
    powerOff,
    info() { return { connected: !!(sim.device && sim.device.gatt.connected), streaming: sim.streaming, packets: sim.packets, commands: sim.commands.slice(), powered: sim.powered, raw: sim.raw }; },
  });
  window.ggSim = sim;

  // --- a small floating panel: drag the slider to grip, or run a scripted hold ---
  let panel = null, out = null, slider = null;
  function renderPanel() { if (out) out.textContent = sim.level.toFixed(1) + " kg"; if (slider && document.activeElement !== slider) slider.value = String(sim.level); }
  function buildPanel() {
    if (panel || !document.body || window.__ggSimNoPanel) return;
    panel = document.createElement("div");
    panel.id = "gg-sim-panel";
    Object.assign(panel.style, {
      position: "fixed", left: "8px", bottom: "8px", zIndex: "50", background: "rgba(20,26,35,0.92)", color: "#fff",
      font: "12px system-ui, sans-serif", padding: "8px 10px", borderRadius: "10px", display: "flex", flexDirection: "column", gap: "6px", width: "190px",
    });
    const title = document.createElement("div");
    title.textContent = "Simulated Progressor";
    Object.assign(title.style, { fontWeight: "600", cursor: "pointer" });
    const body = document.createElement("div");
    Object.assign(body.style, { display: "flex", flexDirection: "column", gap: "6px" });
    title.onclick = () => { body.style.display = body.style.display === "none" ? "flex" : "none"; };
    slider = document.createElement("input");
    slider.type = "range"; slider.min = "0"; slider.max = "60"; slider.step = "0.5"; slider.value = "0";
    slider.oninput = () => { sim.level = +slider.value; sim.holds = []; renderPanel(); };
    out = document.createElement("span");
    const row = (...els) => { const d = document.createElement("div"); Object.assign(d.style, { display: "flex", gap: "4px", flexWrap: "wrap" }); d.append(...els); return d; };
    const btn = (label, f) => { const b = document.createElement("button"); b.type = "button"; b.textContent = label; Object.assign(b.style, { font: "inherit", padding: "3px 6px", borderRadius: "6px", border: "0", minHeight: "0", background: "#3b4656", color: "#fff" }); b.onclick = f; return b; };
    const secs = document.createElement("input");
    secs.type = "number"; secs.value = "8"; secs.min = "1"; secs.max = "300"; Object.assign(secs.style, { width: "48px", padding: "2px", minHeight: "0", font: "inherit" });
    body.append(
      row(slider, out),
      row(btn("Hold", () => sim.hold(+slider.value || 20, +secs.value || 8)), secs, document.createTextNode("s"), btn("Let go", () => sim.release())),
      row(btn("Drop link", () => sim.drop(4000)), btn("Power off", () => sim.powerOff()), btn("Power on", () => sim.powerOn())),
    );
    panel.append(title, body);
    document.body.appendChild(panel);
    renderPanel();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", buildPanel); else buildPanel();
})();
