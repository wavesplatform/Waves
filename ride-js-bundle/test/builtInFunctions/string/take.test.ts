import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("Take", () => {
  const take = `take(callerTestData, ${randomInt()})`
  const takeArgBeforeFunction = `callerTestData.take(${randomInt()})`
  const invalidTake = `take(callerTestData)`
  const invalidTakeArgBeforeFunc = `callerTestData.take(callerTestData, ${randomInt()})`

    test("RIDE-221. function take should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomStringArrayElement(), take], [randomStringArrayElement(), takeArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-222. function take throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), take], [randomUnionArrayElement(), takeArgBeforeFunction], [randomStringArrayElement(), invalidTake], [randomStringArrayElement(), invalidTakeArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
