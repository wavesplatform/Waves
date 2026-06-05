import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultIntegerEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("Size", () => {
  const size = `size(callerTestData)`
  const sizeArgBeforeFunction = `callerTestData.size()`
  const invalidSize = `size()`
  const invalidSizeArgBeforeFunc = `callerTestData.size(callerTestData, callerTestData)`

    test("RIDE-219. function size should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomStringArrayElement(), size], [randomStringArrayElement(), sizeArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)

        }
      }
  });

    test("RIDE-220. function size throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), size], [randomUnionArrayElement(), sizeArgBeforeFunction], [randomStringArrayElement(), invalidSize], [randomStringArrayElement(), invalidSizeArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
