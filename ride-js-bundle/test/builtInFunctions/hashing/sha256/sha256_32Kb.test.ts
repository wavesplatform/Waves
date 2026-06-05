import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Sha256_32Kb", () => {
  const sha256_32Kb = "sha256_32Kb(callerTestData)"
  const sha256_32KbArgBeforeFunc = "callerTestData.sha256_32Kb()"
  const invalidSha256_32Kb = "sha256_32Kb()"
  const invalidSha256_32KbArgBeforeFunc = "callerTestData.sha256_32Kb(callerTestData)"
  const invalidErrorSha256_32Kb = invalidFunctionError("sha256_32Kb", 1)

    test("RIDE-146. Function sha256_32Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sha256_32Kb], [randomByteVectorArrayElement(), sha256_32KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-147. Function sha256_32Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sha256_32Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sha256_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSha256_32Kb, invalidErrorSha256_32Kb], [randomByteVectorArrayElement(), invalidSha256_32KbArgBeforeFunc, invalidErrorSha256_32Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
