import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("DropRight", () => {
  const dropRight = `dropRight(callerTestData, ${randomInt()})`
  const dropRightArgBeforeFunction = `callerTestData.dropRight(${randomInt()})`
  const invalidDropRight = `dropRight(callerTestData)`
  const invalidDropRightArgBeforeFunc = `callerTestData.dropRight(callerTestData, ${randomInt()})`

    test("RIDE-213. function dropRight should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomStringArrayElement(), dropRight], [randomStringArrayElement(), dropRightArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-214. function dropRight throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), dropRight], [randomUnionArrayElement(), dropRightArgBeforeFunction], [randomStringArrayElement(), invalidDropRight], [randomStringArrayElement(), invalidDropRightArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
