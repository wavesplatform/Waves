import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("RsaVerify", () => {
  const rsaVerify = `rsaVerify(${randomDigestAlgorithmTypeArrayElement()}, callerTestData, callerTestData, callerTestData)`
  const rsaVerifyArgBeforeFunc = `${randomDigestAlgorithmTypeArrayElement()}.rsaVerify(callerTestData, callerTestData, callerTestData)`
  const invalidRsaVerify = `rsaVerify()`
  const invalidRsaVerifyArgBeforeFunc = `callerTestData.rsaVerify(callerTestData)`
  const invalidErrorRsaVerify = invalidFunctionError("rsaVerify", 4)

    test("RIDE-237. function rsaVerify should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), rsaVerify], [randomByteVectorArrayElement(), rsaVerifyArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-238. function rsaVerify throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), rsaVerify, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), rsaVerifyArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidRsaVerify, invalidErrorRsaVerify], [randomByteVectorArrayElement(), invalidRsaVerifyArgBeforeFunc, invalidErrorRsaVerify]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
