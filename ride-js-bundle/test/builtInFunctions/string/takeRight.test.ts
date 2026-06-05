import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("TakeRight", () => {
  const takeRight = `takeRight(callerTestData, ${randomInt()})`
  const takeRightArgBeforeFunction = `callerTestData.takeRight(${randomInt()})`
  const invalidTakeRight = `takeRight(callerTestData)`
  const invalidTakeRightArgBeforeFunc = `callerTestData.takeRight(callerTestData, ${randomInt()})`

    test("RIDE-223. function takeRight should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomStringArrayElement(), takeRight], [randomStringArrayElement(), takeRightArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)

        }
      }
  });

    test("RIDE-224. function takeRight throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), takeRight], [randomUnionArrayElement(), takeRightArgBeforeFunction], [randomStringArrayElement(), invalidTakeRight], [randomStringArrayElement(), invalidTakeRightArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
