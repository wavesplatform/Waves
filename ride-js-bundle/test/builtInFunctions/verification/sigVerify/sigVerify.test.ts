import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("SigVerify", () => {
  const sigVerify = `sigVerify(callerTestData, callerTestData, callerTestData)`
  const sigVerifyArgBeforeFunc = `callerTestData.sigVerify(callerTestData, callerTestData)`
  const invalidSigVerify = `sigVerify()`
  const invalidSigVerifyArgBeforeFunc = `callerTestData.sigVerify(callerTestData)`
  const invalidErrorSigVerify = invalidFunctionError("sigVerify", 3)

    test("RIDE-251. function sigVerify should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sigVerify], [randomByteVectorArrayElement(), sigVerifyArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-252. function rsaVerify throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sigVerify, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sigVerifyArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSigVerify, invalidErrorSigVerify], [randomByteVectorArrayElement(), invalidSigVerifyArgBeforeFunc, invalidErrorSigVerify]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
