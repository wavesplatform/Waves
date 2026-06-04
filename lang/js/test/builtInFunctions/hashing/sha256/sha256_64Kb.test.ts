import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Sha256_64Kb", () => {
  const sha256_64Kb = "sha256_64Kb(callerTestData)"
  const sha256_64KbArgBeforeFunc = "callerTestData.sha256_64Kb()"
  const invalidSha256_64Kb = "sha256_64Kb()"
  const invalidSha256_64KbArgBeforeFunc = "callerTestData.sha256_64Kb(callerTestData)"
  const invalidErrorSha256_64Kb = invalidFunctionError("sha256_64Kb", 1)

    test("RIDE-148. Function sha256_64Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sha256_64Kb], [randomByteVectorArrayElement(), sha256_64KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-149. Function sha256_64Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sha256_64Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sha256_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSha256_64Kb, invalidErrorSha256_64Kb], [randomByteVectorArrayElement(), invalidSha256_64KbArgBeforeFunc, invalidErrorSha256_64Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
