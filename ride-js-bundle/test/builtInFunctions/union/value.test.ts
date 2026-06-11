import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { MATCHING_NOT_EXHAUSTIVE, actualVersions, invalidFunctionError } from "../../helpers/testData";

describe("Value", () => {
  const value = "value(callerTestData)"
  const valueArgBeforeFunc = "callerTestData.value()"
  const invalidValue = "value()"
  const invalidValueArgBeforeFunc = "callerTestData.value(callerTestData)"
  const invalidErrorValue = invalidFunctionError("value", 1)

    test("RIDE-230. function value should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), value], [randomInt(), valueArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-231. function value throw a compilation error for can't find overload", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), value, MATCHING_NOT_EXHAUSTIVE], [randomAddressDataArrayElement(), valueArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE], [randomInt(), invalidValue, invalidErrorValue], [randomInt(), invalidValueArgBeforeFunc, invalidErrorValue]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
