import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("Drop", () => {
  const drop = `drop(callerTestData, ${randomInt()})`
  const dropArgBeforeFunction = `callerTestData.drop(${randomInt()})`
  const invalidDrop = `drop(callerTestData)`
  const invalidDropNotInt = `drop(callerTestData, ${randomByteVectorArrayElement()})`
  const invalidDropArgBeforeFunc = `callerTestData.drop(callerTestData, ${randomInt()})`

    test("RIDE-50. Drop function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), drop], [randomByteVectorArrayElement(), dropArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-51. Drop function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), drop], [randomUnionArrayElement(), dropArgBeforeFunction], [randomByteVectorArrayElement(), invalidDrop], [randomByteVectorArrayElement(), invalidDropNotInt], [randomByteVectorArrayElement(), invalidDropArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
