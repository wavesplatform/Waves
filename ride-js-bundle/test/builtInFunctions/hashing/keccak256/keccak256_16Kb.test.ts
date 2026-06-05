import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Keccak256_16Kb", () => {
  const keccak256_16Kb = "keccak256_16Kb(callerTestData)"
  const keccak256_16KbArgBeforeFunc = "callerTestData.keccak256_16Kb()"
  const invalidKeccak256_16Kb = "keccak256_16Kb()"
  const invalidKeccak256_16KbArgBeforeFunc = "callerTestData.keccak256_16Kb(callerTestData)"
  const invalidErrorKeccak256_16Kb = invalidFunctionError("keccak256_16Kb", 1)

    test("RIDE-134. Function keccak256_16Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), keccak256_16Kb], [randomByteVectorArrayElement(), keccak256_16KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-135. Function keccak256_16Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), keccak256_16Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), keccak256_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidKeccak256_16Kb, invalidErrorKeccak256_16Kb], [randomByteVectorArrayElement(), invalidKeccak256_16KbArgBeforeFunc, invalidErrorKeccak256_16Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
