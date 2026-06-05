import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Keccak256_32Kb", () => {
  const keccak256_32Kb = "keccak256_32Kb(callerTestData)"
  const keccak256_32KbArgBeforeFunc = "callerTestData.keccak256_32Kb()"
  const invalidKeccak256_32Kb = "keccak256_32Kb()"
  const invalidKeccak256_32KbArgBeforeFunc = "callerTestData.keccak256_32Kb(callerTestData)"
  const invalidErrorKeccak256_32Kb = invalidFunctionError("keccak256_32Kb", 1)

    test("RIDE-136. Function keccak256_32Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), keccak256_32Kb], [randomByteVectorArrayElement(), keccak256_32KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-137. Function keccak256_32Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), keccak256_32Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), keccak256_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidKeccak256_32Kb, invalidErrorKeccak256_32Kb], [randomByteVectorArrayElement(), invalidKeccak256_32KbArgBeforeFunc, invalidErrorKeccak256_32Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
