import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("RsaVerify_64Kb", () => {
  const rsaVerify_64Kb = `rsaVerify_64Kb(${randomDigestAlgorithmTypeArrayElement()}, callerTestData, callerTestData, callerTestData)`
  const rsaVerify_64KbArgBeforeFunc = `${randomDigestAlgorithmTypeArrayElement()}.rsaVerify_64Kb(callerTestData, callerTestData, callerTestData)`
  const invalidRsaVerify_64Kb = "rsaVerify_64Kb()"
  const invalidRsaVerify_64KbArgBeforeFunc = "callerTestData.rsaVerify_64Kb(callerTestData)"
  const invalidErrorRsaVerify_64Kb = invalidFunctionError("rsaVerify_64Kb", 4)

    test("RIDE-245. rsaVerify_64Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), rsaVerify_64Kb], [randomByteVectorArrayElement(), rsaVerify_64KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-246. rsaVerify_64Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), rsaVerify_64Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), rsaVerify_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidRsaVerify_64Kb, invalidErrorRsaVerify_64Kb], [randomByteVectorArrayElement(), invalidRsaVerify_64KbArgBeforeFunc, invalidErrorRsaVerify_64Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-247. Can't find a function rsaVerify_64Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), rsaVerify_64Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
