// Tests exercise the real shipped public API of @waves/ride-js (the wrapped glue),
// which loads ./interop (real @waves/ts-lib-crypto globals) and the single combined
// ScalaJS artifact. We require the built CommonJS entry (dist/index.js): the glue
// uses require/module.exports/global, so it must be consumed as CJS, not the TS
// source. `pnpm test` runs `pretest` (build:ts) to produce it; the ScalaJS artifact
// comes from `sbt replJS/fullOptJS`.
import { createRequire } from "module";

const require = createRequire(import.meta.url);
// The one place tests load the package; api.test.ts checks the raw API through it too.
export const ride: any = require("../../dist/index.js");

// The public `compile` wraps a successful result under `.result` ({ bytes, base64,
// size, ast, complexity, verifierComplexity, callableComplexities, ... }); on
// failure it returns a flat `{ error }`. `parseAndCompile` is a raw passthrough.
export interface CompileResult {
  error?: unknown;
  result?: any;
  [key: string]: any;
}

export interface ParseAndCompileResult {
  exprAst?: any;
  dAppAst?: any;
  ast?: any;
  errorList?: any[];
  error?: unknown;
  complexity?: number;
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
