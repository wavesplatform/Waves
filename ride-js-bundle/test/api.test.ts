// Smoke tests for the wrapped @waves/ride-js public API (the glue layer), which the
// compiler-focused builtInFunctions suite does not exercise directly. Guards the
// shapes that consumers depend on for API compatibility.
import { describe, test, expect } from "vitest";
import type { ICompilationResult, IFlattenedCompilationResult } from "@waves/ride-js";
import { ride } from "./helpers/compiler";
import { allStdLibVersions, dApp } from "./helpers/jsTestBase";

// Record<keyof T, true> fails tsc on a key the typings lack or declare extra, so these mirror src/index.d.ts exactly.
const declaredResultKeys: Record<keyof ICompilationResult["result"], true> = {
  ast: true,
  base64: true,
  bytes: true,
  size: true,
  complexity: true,
  verifierComplexity: true,
  callableComplexities: true,
  userFunctionComplexities: true,
  globalVariableComplexities: true
};
const declaredFlatKeys: Record<Exclude<keyof IFlattenedCompilationResult, "error">, true> = declaredResultKeys;

const dAppWithEverything = dApp(
  `
let g = 1
func f() = g
@Callable(i)
func c() = [IntegerEntry("k", f())]
@Verifier(tx)
func v() = true
`,
  6
);

describe("ride-js public API", () => {
  test("compile wraps a successful result under .result with bytes/base64/size/complexity", () => {
    const r = ride.compile("true");
    expect(r.error).toBeUndefined();
    expect(r.result.bytes).toBeInstanceOf(Uint8Array);
    expect(typeof r.result.base64).toBe("string");
    expect(r.result.size).toBe(r.result.bytes.byteLength);
    expect(typeof r.result.complexity).toBe("number");
  });

  test("compile returns a flat { error } on failure", () => {
    const r = ride.compile("1 + 1");
    expect(r.result).toBeUndefined();
    expect(String(r.error)).toContain("Script should return boolean");
  });

  test("compile rejects non-string input", () => {
    const r = ride.compile(42 as unknown as string);
    expect(r.error).toBe("Type error: contract should be string");
  });

  test("flattenCompilationResult unwraps a success to a flat object", () => {
    const flat = ride.flattenCompilationResult(ride.compile("true"));
    expect(typeof flat.base64).toBe("string");
    expect(typeof flat.complexity).toBe("number");
    expect(flat.error).toBeUndefined();
  });

  test("repl() exposes evaluate/info/totalInfo/clear/reconfigure", () => {
    const repl = ride.repl();
    for (const k of ["evaluate", "info", "totalInfo", "clear", "reconfigure"]) {
      expect(typeof repl[k]).toBe("function");
    }
  });

  test("version is a string and contractLimits exposes expected members", () => {
    expect(typeof ride.version).toBe("string");
    expect(typeof ride.contractLimits.MaxExprSizeInBytes).toBe("number");
    expect(typeof ride.contractLimits.MaxComplexityByVersion).toBe("function");
  });

  test("passthroughs are present", () => {
    for (const k of ["scriptInfo", "getTypes", "getVarsDoc", "getFunctionsDoc", "decompile", "parseAndCompile"]) {
      expect(typeof ride[k]).toBe("function");
    }
  });

  test("compile result matches the published typings", () => {
    const r = ride.compile(dAppWithEverything);
    expect(r.error).toBeUndefined();
    expect(Object.keys(r.result).sort()).toEqual(Object.keys(declaredResultKeys).sort());
    expect(Object.keys(ride.flattenCompilationResult(r)).sort()).toEqual(Object.keys(declaredFlatKeys).sort());
  });

  // allStdLibVersions is hand-kept; this fails when the compiler gains a version the list lacks.
  test("allStdLibVersions lists every version the compiler accepts", () => {
    const max = Math.max(...allStdLibVersions);
    const withVersion = (v: number) => `{-# STDLIB_VERSION ${v} #-}\n{-# CONTENT_TYPE EXPRESSION #-}\ntrue`;
    expect(ride.compile(withVersion(max)).error).toBeUndefined();
    expect(String(ride.compile(withVersion(max + 1)).error)).toContain("Illegal directive value");
  });
});
