// Tests exercise the real shipped public API of @waves/ride-js (the wrapped glue),
// which loads ./interop (real @waves/ts-lib-crypto globals) and the single combined
// ScalaJS artifact. We require the built CommonJS entry (dist/index.js) — the glue
// uses require/module.exports/global, so it must be consumed as CJS, not the TS
// source. `npm test` runs `pretest` (build:scalajs + build:ts) to produce it.
import { createRequire } from "module";

const require = createRequire(import.meta.url);
// eslint-disable-next-line @typescript-eslint/no-var-requires
const ride = require("../../dist/index.js");

// The public `compile` wraps a successful result under `.result` ({ bytes, base64,
// size, ast, complexity, verifierComplexity, callableComplexities, ... }); on
// failure it returns a flat `{ error }`. `parseAndCompile`/`decompile`/`scriptInfo`
// are raw passthroughs.
export interface CompileResult {
  error?: unknown;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  result?: any;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  [key: string]: any;
}

export interface ParseAndCompileResult {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  exprAst?: any;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  dAppAst?: any;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  ast?: any;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  errorList?: any[];
  error?: unknown;
  complexity?: number;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  [key: string]: any;
}

export function compile(
  input: string,
  estimatorVersion: number,
  needCompaction = false,
  removeUnusedCode = false,
  libraries: Record<string, string> = {}
): CompileResult {
  return ride.compile(input, estimatorVersion, needCompaction, removeUnusedCode, libraries);
}

export function parseAndCompile(
  input: string,
  estimatorVersion: number,
  needCompaction = false,
  removeUnusedCode = false,
  libraries: Record<string, string> = {}
): ParseAndCompileResult {
  return ride.parseAndCompile(input, estimatorVersion, needCompaction, removeUnusedCode, libraries);
}

export function decompile(input: string): { result?: string; error?: string } {
  return ride.decompile(input);
}

export function scriptInfo(input: string): Record<string, unknown> {
  return ride.scriptInfo(input);
}
