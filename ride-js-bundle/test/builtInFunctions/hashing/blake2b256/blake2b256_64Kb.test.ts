import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Blake2b256_64Kb", () => {
  const blake2b256_64Kb = "blake2b256_64Kb(callerTestData)"
  const blake2b256_64KbArgBeforeFunc = "callerTestData.blake2b256_64Kb()"
  const invalidBlake2b256_64Kb = "blake2b256_64Kb()"
  const invalidBlake2b256_64KbArgBeforeFunc = "callerTestData.blake2b256_64Kb(callerTestData)"
  const invalidErrorBlake2b256_64Kb = invalidFunctionError("blake2b256_64Kb", 1)

    test("RIDE-128. Function blake2b256_64Kb should compile for valid ByteVector", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), blake2b256_64Kb], [randomByteVectorArrayElement(), blake2b256_64KbArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-128. Function blake2b256_64Kb должна выдавать ошибку при невалидных данных", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), blake2b256_64Kb, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), blake2b256_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidBlake2b256_64Kb, invalidErrorBlake2b256_64Kb], [randomByteVectorArrayElement(), invalidBlake2b256_64KbArgBeforeFunc, invalidErrorBlake2b256_64Kb]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
