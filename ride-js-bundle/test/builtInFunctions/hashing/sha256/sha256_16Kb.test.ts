import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Sha256_16Kb", () => {
  const sha256_16Kb = "sha256_16Kb(callerTestData)"
  const sha256_16KbArgBeforeFunc = "callerTestData.sha256_16Kb()"
  const invalidSha256_16Kb = "sha256_16Kb()"
  const invalidSha256_16KbArgBeforeFunc = "callerTestData.sha256_16Kb(callerTestData)"
  const invalidErrorSha256_16Kb = invalidFunctionError("sha256_16Kb", 1)

    test("RIDE-144. Function sha256_16Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sha256_16Kb], [randomByteVectorArrayElement(), sha256_16KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-145. Function sha256_16Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sha256_16Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sha256_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSha256_16Kb, invalidErrorSha256_16Kb], [randomByteVectorArrayElement(), invalidSha256_16KbArgBeforeFunc, invalidErrorSha256_16Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
