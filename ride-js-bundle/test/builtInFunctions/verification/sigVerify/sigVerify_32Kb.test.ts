import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("SigVerify_32Kb", () => {
  const sigVerify_32Kb = `sigVerify_32Kb(callerTestData, callerTestData, callerTestData)`
  const sigVerify_32KbArgBeforeFunc = `callerTestData.sigVerify_32Kb(callerTestData, callerTestData)`
  const invalidSigVerify_32Kb = "sigVerify_32Kb()"
  const invalidSigVerify_32KbArgBeforeFunc = "callerTestData.sigVerify_32Kb(callerTestData)"
  const invalidErrorSigVerify_32Kb = invalidFunctionError("sigVerify_32Kb", 3)

    test("RIDE-256. sigVerify_32Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sigVerify_32Kb], [randomByteVectorArrayElement(), sigVerify_32KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-257. sigVerify_32Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sigVerify_32Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sigVerify_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSigVerify_32Kb, invalidErrorSigVerify_32Kb], [randomByteVectorArrayElement(), invalidSigVerify_32KbArgBeforeFunc, invalidErrorSigVerify_32Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-258. Can't find a function sigVerify_32Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), sigVerify_32Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
