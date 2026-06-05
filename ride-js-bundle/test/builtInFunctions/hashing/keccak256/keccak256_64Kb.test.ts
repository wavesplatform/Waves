import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Keccak256_64Kb", () => {
  const keccak256_64Kb = "keccak256_64Kb(callerTestData)"
  const keccak256_64KbArgBeforeFunc = "callerTestData.keccak256_64Kb()"
  const invalidKeccak256_64Kb = "keccak256_64Kb()"
  const invalidKeccak256_64KbArgBeforeFunc = "callerTestData.keccak256_64Kb(callerTestData)"
  const invalidErrorKeccak256_64Kb = invalidFunctionError("keccak256_64Kb", 1)

    test("RIDE-138. Function keccak256_64Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), keccak256_64Kb], [randomByteVectorArrayElement(), keccak256_64KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-139. Function keccak256_64Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), keccak256_64Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), keccak256_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidKeccak256_64Kb, invalidErrorKeccak256_64Kb], [randomByteVectorArrayElement(), invalidKeccak256_64KbArgBeforeFunc, invalidErrorKeccak256_64Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
