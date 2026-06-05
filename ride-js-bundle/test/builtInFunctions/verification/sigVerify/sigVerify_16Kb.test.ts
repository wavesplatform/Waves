import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("SigVerify_16Kb", () => {
  const sigVerify_16Kb = `sigVerify_16Kb(callerTestData, callerTestData, callerTestData)`
  const sigVerify_16KbArgBeforeFunc = `callerTestData.sigVerify_16Kb(callerTestData, callerTestData)`
  const invalidSigVerify_16Kb = "sigVerify_16Kb()"
  const invalidSigVerify_16KbArgBeforeFunc = "callerTestData.sigVerify_16Kb(callerTestData)"
  const invalidErrorSigVerify_16Kb = invalidFunctionError("sigVerify_16Kb", 3)

    test("RIDE-253. sigVerify_16Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sigVerify_16Kb], [randomByteVectorArrayElement(), sigVerify_16KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-254. sigVerify_16Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sigVerify_16Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sigVerify_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSigVerify_16Kb, invalidErrorSigVerify_16Kb], [randomByteVectorArrayElement(), invalidSigVerify_16KbArgBeforeFunc, invalidErrorSigVerify_16Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-255. Can't find a function sigVerify_16Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), sigVerify_16Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
