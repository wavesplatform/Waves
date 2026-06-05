import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomBoolean, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ToBytes", () => {
  // toBytes
  const toBytes = "toBytes(callerTestData)"
  const toBytesArgBeforeFunc = "callerTestData.toBytes()"
  const invalidToBytes = "toBytes()"
  const invalidToBytesArgBeforeFunc = "callerTestData.toBytes(callerTestData)"

    test("RIDE-74. toBytes function should compile for valid values int, string, boolean", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomStringArrayElement(), toBytes], [randomInt(), toBytes], [randomBoolean(), toBytes], [randomStringArrayElement(), toBytesArgBeforeFunc], [randomInt(), toBytesArgBeforeFunc], [randomBoolean(), toBytesArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-75. toBytes function should compile with bigInt for V5, V6 versions", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[`toBigInt(${randomInt()})`, toBytes], [`toBigInt(${randomInt()})`, toBytesArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-76. toBytes function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomAddressDataArrayElement(), toBytes, CANT_FIND_A_FUNCTION_OVERLOAD], [randomUnionArrayElement(), toBytesArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomInt(), invalidToBytes, CANT_FIND_A_FUNCTION_OVERLOAD], [randomStringArrayElement(), invalidToBytesArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
