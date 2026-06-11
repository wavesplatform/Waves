// Port of com/wavesplatform/JsTestBase.scala
// Assertion helpers + script wrappers built on the public compiler API.
import { expect } from "vitest";
import { compile } from "./compiler";

// StdLibVersion collapses to its numeric id in JS.
export const V1 = 1;
export const V2 = 2;
export const V3 = 3;
export const V4 = 4;
export const V5 = 5;
export const V6 = 6;
export const V7 = 7;
export const V8 = 8;
export const V9 = 9;

// Mirrors DirectiveDictionary[StdLibVersion].all (all defined library versions).
export const allStdLibVersions: number[] = [V1, V2, V3, V4, V5, V6, V7, V8, V9];

export interface DAppComplexitiesResult {
  complexity: number;
  verifierComplexity: number;
  callableComplexities: Record<string, number>;
  userFunctionComplexities: Record<string, number>;
  globalVariableComplexities: Record<string, number>;
}

export function assertCompileError(code: string, expectingError: string, estimator = 3): void {
  const error = compile(code, estimator).error;
  expect(String(error)).toContain(expectingError);
}

export function assertCompileErrorDApp(code: string, version: number, expectingError: string, estimator = 3): void {
  const error = compile(dApp(code, version), estimator).error;
  expect(String(error)).toContain(expectingError);
}

export function assertCompileErrorExpression(code: string, version: number, expectingError: string, estimator = 3): void {
  const error = compile(expression(code, version), estimator).error;
  expect(String(error)).toContain(expectingError);
}

export function assertCompileSuccess(code: string, estimator = 3): void {
  const error = compile(code, estimator).error;
  expect(error).toBeUndefined();
}

export function assertCompileSuccessDApp(code: string, version: number, estimator = 3): void {
  const error = compile(dApp(code, version), estimator).error;
  expect(error).toBeUndefined();
}

export function assertCompileSuccessExpression(code: string, version: number, estimator = 3): void {
  const error = compile(expression(code, version), estimator).error;
  expect(error).toBeUndefined();
}

export function expressionComplexity(code: string, version = V6, estimator = 3): number {
  // The wrapped public `compile` nests a successful result under `.result`.
  return compile(expression(code, version), estimator).result.complexity as number;
}

export function dAppComplexities(code: string, version = V6, estimator = 3): DAppComplexitiesResult {
  const { result } = compile(dApp(code, version), estimator);
  return {
    complexity: result.complexity as number,
    verifierComplexity: result.verifierComplexity as number,
    callableComplexities: result.callableComplexities as Record<string, number>,
    userFunctionComplexities: result.userFunctionComplexities as Record<string, number>,
    globalVariableComplexities: result.globalVariableComplexities as Record<string, number>
  };
}

export function expression(code: string, version: number): string {
  return `
{-# STDLIB_VERSION ${version} #-}
{-# CONTENT_TYPE   EXPRESSION    #-}
{-# SCRIPT_TYPE    ACCOUNT       #-}

${code}
     `;
}

export function dApp(code: string, version: number): string {
  return `
{-# STDLIB_VERSION ${version} #-}
{-# CONTENT_TYPE   DAPP          #-}
{-# SCRIPT_TYPE    ACCOUNT       #-}

${code}
     `;
}
