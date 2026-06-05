import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Blake2b256", () => {
  const blake2b256 = "blake2b256(callerTestData)"
  const blake2b256ArgBeforeFunc = "callerTestData.blake2b256()"
  const invalidBlake2b256 = "blake2b256()"
  const invalidBlake2b256ArgBeforeFunc = "callerTestData.blake2b256(callerTestData)"
  const invalidErrorBlake2b256 = invalidFunctionError("blake2b256", 1)

    test("RIDE-122. Function blake2b256 should compile for valid ByteVector", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), blake2b256], [randomByteVectorArrayElement(), blake2b256ArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-123. Function blake2b256 should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), blake2b256, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), blake2b256ArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidBlake2b256, invalidErrorBlake2b256], [randomByteVectorArrayElement(), invalidBlake2b256ArgBeforeFunc, invalidErrorBlake2b256]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
