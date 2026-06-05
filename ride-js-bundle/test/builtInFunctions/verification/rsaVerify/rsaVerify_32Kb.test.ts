import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("RsaVerify_32Kb", () => {
  const rsaVerify_32Kb = `rsaVerify_32Kb(${randomDigestAlgorithmTypeArrayElement()}, callerTestData, callerTestData, callerTestData)`
  const rsaVerify_32KbArgBeforeFunc = `${randomDigestAlgorithmTypeArrayElement()}.rsaVerify_32Kb(callerTestData, callerTestData, callerTestData)`
  const invalidRsaVerify_32Kb = "rsaVerify_32Kb()"
  const invalidRsaVerify_32KbArgBeforeFunc = "callerTestData.rsaVerify_32Kb(callerTestData)"
  const invalidErrorRsaVerify_32Kb = invalidFunctionError("rsaVerify_32Kb", 4)

    test("RIDE-242. rsaVerify_32Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), rsaVerify_32Kb], [randomByteVectorArrayElement(), rsaVerify_32KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-243. rsaVerify_32Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), rsaVerify_32Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), rsaVerify_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidRsaVerify_32Kb, invalidErrorRsaVerify_32Kb], [randomByteVectorArrayElement(), invalidRsaVerify_32KbArgBeforeFunc, invalidErrorRsaVerify_32Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-244. Can't find a function rsaVerify_32Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), rsaVerify_32Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
