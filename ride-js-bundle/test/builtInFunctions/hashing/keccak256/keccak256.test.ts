import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../../helpers/randomData";
import { GreaterV3ResultBinaryEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../../helpers/testData";

describe("Keccak256", () => {
  const keccak256 = "keccak256(callerTestData)"
  const keccak256ArgBeforeFunc = "callerTestData.keccak256()"
  const invalidKeccak256 = "keccak256()"
  const invalidKeccak256ArgBeforeFunc = "callerTestData.keccak256(callerTestData)"
  const invalidErrorKeccak256 = invalidFunctionError("keccak256", 1)

    test("RIDE-132. Function Keccak256 should compile for valid ByteVector", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), keccak256], [randomByteVectorArrayElement(), keccak256ArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-133. Function blake2b256 should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), keccak256, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), keccak256ArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidKeccak256, invalidErrorKeccak256], [randomByteVectorArrayElement(), invalidKeccak256ArgBeforeFunc, invalidErrorKeccak256]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
