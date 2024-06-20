import { WASI } from "@cloudflare/workers-wasi";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import wasm_module from "./handlers.wasm";

const wasi = new WASI();

const instance_exports = {};
const { instance } = await WebAssembly.Instance(wasm_module, {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
const handlers = await instance.exports.handlers();

export default handlers;
