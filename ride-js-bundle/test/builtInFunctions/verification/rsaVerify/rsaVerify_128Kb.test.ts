import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("RsaVerify_128Kb", () => {
  const rsaVerify_128Kb = `rsaVerify_128Kb(${randomDigestAlgorithmTypeArrayElement()}, callerTestData, callerTestData, callerTestData)`
  const rsaVerify_128KbArgBeforeFunc = `${randomDigestAlgorithmTypeArrayElement()}.rsaVerify_128Kb(callerTestData, callerTestData, callerTestData)`
  const invalidRsaVerify_128Kb = "rsaVerify_128Kb()"
  const invalidRsaVerify_128KbArgBeforeFunc = "callerTestData.rsaVerify_128Kb(callerTestData)"
  const invalidErrorRsaVerify_128Kb = invalidFunctionError("rsaVerify_128Kb", 4)

    test("RIDE-248. rsaVerify_128Kb function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), rsaVerify_128Kb], [randomByteVectorArrayElement(), rsaVerify_128KbArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-249. rsaVerify_128Kb function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), rsaVerify_128Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), rsaVerify_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidRsaVerify_128Kb, invalidErrorRsaVerify_128Kb], [randomByteVectorArrayElement(), invalidRsaVerify_128KbArgBeforeFunc, invalidErrorRsaVerify_128Kb]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-250. Can't find a function rsaVerify_128Kb for RIDE V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), rsaVerify_128Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
