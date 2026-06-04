// Typed wrapper around the published CommonJS artifact (dist/lang.js).
// This is the real public surface of @waves/ride-lang that the tests exercise.
import { createRequire } from "module";

// The compiled RIDE compiler (dist/lang.js) resolves a set of crypto primitives
// from the global scope (see com.wavesplatform.lang.impl.Global). During
// compilation only the hashing functions are actually invoked (e.g. to derive
// script ids/checksums); the original Scala/utest suite injected identity stubs
// for blake2b256/keccak256. We mirror that here, injecting BEFORE requiring the
// artifact so the free-variable lookups resolve. Tests assert compiler output,
// never cryptographic results, so identity/true stubs are sufficient.
const g = globalThis as unknown as Record<string, unknown>;
const identity = (message: ArrayBuffer): ArrayBuffer => message;
g.blake2b256 = identity;
g.keccak256 = identity;
g.sha256 = identity;
g.curve25519verify = (): boolean => true;
g.rsaVerify = (): boolean => true;
g.merkleVerify = (): boolean => true;

const require = createRequire(import.meta.url);
// eslint-disable-next-line @typescript-eslint/no-var-requires
const lang = require("../../dist/lang.js");

export interface CompileResult {
  error?: unknown;
  complexity?: number;
  verifierComplexity?: number;
  callableComplexities?: Record<string, number>;
  userFunctionComplexities?: Record<string, number>;
  globalVariableComplexities?: Record<string, number>;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  ast?: any;
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
  return lang.compile(input, estimatorVersion, needCompaction, removeUnusedCode, libraries);
}

export function parseAndCompile(
  input: string,
  estimatorVersion: number,
  needCompaction = false,
  removeUnusedCode = false,
  libraries: Record<string, string> = {}
): ParseAndCompileResult {
  return lang.parseAndCompile(input, estimatorVersion, needCompaction, removeUnusedCode, libraries);
}

export function decompile(input: string): { result?: string; error?: string } {
  return lang.decompile(input);
}

export function scriptInfo(input: string): Record<string, unknown> {
  return lang.scriptInfo(input);
}
