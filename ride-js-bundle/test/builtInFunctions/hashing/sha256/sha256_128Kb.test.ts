import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Sha256_128Kb", () => {
  const sha256_128Kb = "sha256_128Kb(callerTestData)"
  const sha256_128KbArgBeforeFunc = "callerTestData.sha256_128Kb()"
  const invalidSha256_128Kb = "sha256_128Kb()"
  const invalidSha256_128KbArgBeforeFunc = "callerTestData.sha256_128Kb(callerTestData)"
  const invalidErrorSha256_128Kb = invalidFunctionError("sha256_128Kb", 1)

    test("RIDE-150. Function sha256_128Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), sha256_128Kb], [randomByteVectorArrayElement(), sha256_128KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-151. Function sha256_128Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), sha256_128Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), sha256_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidSha256_128Kb, invalidErrorSha256_128Kb], [randomByteVectorArrayElement(), invalidSha256_128KbArgBeforeFunc, invalidErrorSha256_128Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
