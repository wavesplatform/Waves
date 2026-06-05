import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("SigVerify_64Kb", () => {
  const sigVerify_64Kb = `sigVerify_64Kb(callerTestData, callerTestData, callerTestData)`
  const sigVerify_64KbArgBeforeFunc = `callerTestData.sigVerify_64Kb(callerTestData, callerTestData)`
  const invalidSigVerify_64Kb = "sigVerify_64Kb()"
  const invalidSigVerify_64KbArgBeforeFunc = "callerTestData.sigVerify_64Kb(callerTestData)"
  const invalidErrorSigVerify_64Kb = invalidFunctionError("sigVerify_64Kb", 3)

    test("RIDE-259. sigVerify_64Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sigVerify_64Kb], [randomByteVectorArrayElement(), sigVerify_64KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-260. sigVerify_64Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sigVerify_64Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sigVerify_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSigVerify_64Kb, invalidErrorSigVerify_64Kb], [randomByteVectorArrayElement(), invalidSigVerify_64KbArgBeforeFunc, invalidErrorSigVerify_64Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-261. Can't find a function sigVerify_64Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), sigVerify_64Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
