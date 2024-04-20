import Context from "https://deno.land/std@0.206.0/wasi/snapshot_preview1.ts";
import * as wasi from "https://deno.land/std@0.206.0/wasi/snapshot_preview1.ts";
import * as ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const context = new Context({});
const wasm = Deno.args[0];
const binary = await Deno.readFile(wasm);
const module = await WebAssembly.compile(binary);
const __exports = {};
const instance = await WebAssembly.instantiate(module, {
  wasi_snapshot_preview1: context.exports,
  ghc_wasm_jsffi: ghc_wasm_jsffi.default(__exports),
});

Object.assign(__exports, instance.exports);

// This function is a part of GHC's RTS API. It must be called before
// any other exported Haskell functions are called.
context.initialize(instance);

const hs_start = instance.exports.hs_start as () => Promise<void>;
await hs_start();
