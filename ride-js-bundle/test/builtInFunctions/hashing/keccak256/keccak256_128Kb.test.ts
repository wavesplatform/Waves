import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Keccak256_128Kb", () => {
  const keccak256_128Kb = "keccak256_128Kb(callerTestData)"
  const keccak256_128KbArgBeforeFunc = "callerTestData.keccak256_128Kb()"
  const invalidKeccak256_128Kb = "keccak256_128Kb()"
  const invalidKeccak256_128KbArgBeforeFunc = "callerTestData.keccak256_128Kb(callerTestData)"
  const invalidErrorKeccak256_128Kb = invalidFunctionError("keccak256_128Kb", 1)

    test("RIDE-140. Function keccak256_128Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), keccak256_128Kb], [randomByteVectorArrayElement(), keccak256_128KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-141. Function keccak256_128Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), keccak256_128Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), keccak256_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidKeccak256_128Kb, invalidErrorKeccak256_128Kb], [randomByteVectorArrayElement(), invalidKeccak256_128KbArgBeforeFunc, invalidErrorKeccak256_128Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
