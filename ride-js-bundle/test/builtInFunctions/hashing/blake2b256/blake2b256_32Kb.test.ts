import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Blake2b256_32Kb", () => {
  const blake2b256_32Kb = "blake2b256_32Kb(callerTestData)"
  const blake2b256_32KbArgBeforeFunc = "callerTestData.blake2b256_32Kb()"
  const invalidBlake2b256_32Kb = "blake2b256_32Kb()"
  const invalidBlake2b256_32KbArgBeforeFunc = "callerTestData.blake2b256_32Kb(callerTestData)"
  const invalidErrorBlake2b256_32Kb = invalidFunctionError("blake2b256_32Kb", 1)

    test("RIDE-126. Function blake2b256 should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), blake2b256_32Kb], [randomByteVectorArrayElement(), blake2b256_32KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-127. Function blake2b256_32Kb should throw an error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), blake2b256_32Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), blake2b256_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidBlake2b256_32Kb, invalidErrorBlake2b256_32Kb], [randomByteVectorArrayElement(), invalidBlake2b256_32KbArgBeforeFunc, invalidErrorBlake2b256_32Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
