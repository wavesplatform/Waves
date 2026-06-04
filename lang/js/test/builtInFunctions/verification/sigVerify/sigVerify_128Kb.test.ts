import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("SigVerify_128Kb", () => {
  const sigVerify_128Kb = `sigVerify_128Kb(callerTestData, callerTestData, callerTestData)`
  const sigVerify_128KbArgBeforeFunc = `callerTestData.sigVerify_128Kb(callerTestData, callerTestData)`
  const invalidSigVerify_128Kb = "sigVerify_128Kb()"
  const invalidSigVerify_128KbArgBeforeFunc = "callerTestData.sigVerify_128Kb(callerTestData)"
  const invalidErrorSigVerify_128Kb = invalidFunctionError("sigVerify_128Kb", 3)

    test("RIDE-262. sigVerify_128Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sigVerify_128Kb], [randomByteVectorArrayElement(), sigVerify_128KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-263. sigVerify_128Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sigVerify_128Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sigVerify_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSigVerify_128Kb, invalidErrorSigVerify_128Kb], [randomByteVectorArrayElement(), invalidSigVerify_128KbArgBeforeFunc, invalidErrorSigVerify_128Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-264. Can't find a function sigVerify_128Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), sigVerify_128Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
