import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("Drop", () => {
  const drop = `drop(callerTestData, ${randomInt()})`
  const dropArgBeforeFunction = `callerTestData.drop(${randomInt()})`
  const invalidDrop = `drop(callerTestData)`
  const invalidDropArgBeforeFunc = `callerTestData.drop(callerTestData, ${randomInt()})`

    test("RIDE-211. function drop should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomStringArrayElement(), drop], [randomStringArrayElement(), dropArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-212. function drop throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), drop], [randomUnionArrayElement(), dropArgBeforeFunction], [randomStringArrayElement(), invalidDrop], [randomStringArrayElement(), invalidDropArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
