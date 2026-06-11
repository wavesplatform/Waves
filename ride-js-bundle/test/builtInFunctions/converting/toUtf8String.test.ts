import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { GreaterV3ResultStringEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../helpers/testData";

describe("ToUtf8String", () => {
  const toUtf8String = "toUtf8String(callerTestData)"
  const toUtf8StringArgBeforeFunc = "callerTestData.toUtf8String()"
  const invalidToUtf8String = "toUtf8String()"
  const invalidToUtf8StringArgBeforeFunc = "callerTestData.toUtf8String(callerTestData)"

    test("RIDE-83. Functions toUtf8String function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), toUtf8String], [randomByteVectorArrayElement(), toUtf8StringArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-84. toUtf8String function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), toUtf8String, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), toUtf8StringArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidToUtf8String, invalidFunctionError("toUtf8String", 1)], [randomByteVectorArrayElement(), invalidToUtf8StringArgBeforeFunc, invalidFunctionError("toUtf8String", 1)]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
