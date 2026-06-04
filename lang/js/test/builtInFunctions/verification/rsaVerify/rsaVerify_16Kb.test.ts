import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("RsaVerify_16Kb", () => {
  const rsaVerify_16Kb = `rsaVerify_16Kb(${randomDigestAlgorithmTypeArrayElement()}, callerTestData, callerTestData, callerTestData)`
  const rsaVerify_16KbArgBeforeFunc = `${randomDigestAlgorithmTypeArrayElement()}.rsaVerify_16Kb(callerTestData, callerTestData, callerTestData)`
  const invalidRsaVerify_16Kb = "rsaVerify_16Kb()"
  const invalidRsaVerify_16KbArgBeforeFunc = "callerTestData.rsaVerify_16Kb(callerTestData)"
  const invalidErrorRsaVerify_16Kb = invalidFunctionError("rsaVerify_16Kb", 4)

    test("RIDE-239. rsaVerify_16Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), rsaVerify_16Kb], [randomByteVectorArrayElement(), rsaVerify_16KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-240. rsaVerify_16Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), rsaVerify_16Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), rsaVerify_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidRsaVerify_16Kb, invalidErrorRsaVerify_16Kb], [randomByteVectorArrayElement(), invalidRsaVerify_16KbArgBeforeFunc, invalidErrorRsaVerify_16Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-241. Can't find a function makeString_2C for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), rsaVerify_16Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
