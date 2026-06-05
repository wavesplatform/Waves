import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Sha256", () => {
  const sha256 = "sha256(callerTestData)"
  const sha256ArgBeforeFunc = "callerTestData.sha256()"
  const invalidSha256 = "sha256()"
  const invalidSha256ArgBeforeFunc = "callerTestData.sha256(callerTestData)"
  const invalidErrorSha256 = invalidFunctionError("sha256", 1)

    test("RIDE-142. Function sha256 should compile for valid ByteVector", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sha256], [randomByteVectorArrayElement(), sha256ArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-143. Function sha256 should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sha256, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sha256ArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSha256, invalidErrorSha256], [randomByteVectorArrayElement(), invalidSha256ArgBeforeFunc, invalidErrorSha256]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
