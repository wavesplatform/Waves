import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { GreaterV3ResultIntegerEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result } from "../../helpers/testData";

describe("ParseInt", () => {
  // parseInt
  const parseInt = "parseInt(callerTestData)"
  const parseIntArgBeforeFunc = "callerTestData.parseInt()"
  const invalidParseInt = "parseInt()"
  const invalidParseIntArgBeforeFunc = "callerTestData.parseInt(callerTestData)"

  // parseIntValue
  const parseIntValue = "parseIntValue(callerTestData)"
  const parseIntValueArgBeforeFunc = "callerTestData.parseIntValue()"
  const invalidParseIntValue = "parseIntValue()"
  const invalidParseIntValueArgBeforeFunc = "callerTestData.parseIntValue(callerTestData)"

    test("RIDE-68. ParseInt function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomStringArrayElement(), parseInt], [randomStringArrayElement(), parseIntArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-69. ParseInt function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func, error] of [[randomInt(), parseInt, nonMatchingTypes("String")], [randomAliasDataArrayElement(), parseIntArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), invalidParseInt, invalidFunctionError("parseInt", 1)], [randomStringArrayElement(), invalidParseIntArgBeforeFunc, invalidFunctionError("parseInt", 1)]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-70. parseIntValue function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomStringArrayElement(), parseIntValue], [randomStringArrayElement(), parseIntValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-71. parseIntValue function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func, error] of [[randomInt(), parseIntValue, nonMatchingTypes("String")], [randomAliasDataArrayElement(), parseIntValueArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), invalidParseIntValue, invalidFunctionError("parseIntValue", 1)], [randomStringArrayElement(), invalidParseIntValueArgBeforeFunc, invalidFunctionError("parseIntValue", 1)]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
