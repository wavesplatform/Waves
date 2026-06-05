import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomBoolean, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { UNDEFINED_TYPE, invalidFunctionError, nonMatchingTypes, oldVersions, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ParseBigInt", () => {
  const parseBigInt = "parseBigInt(callerTestData)"
  const parseBigIntArgBeforeFunc = "callerTestData.parseBigInt()"
  const invalidFunctionParseBigInt = "parseBigInt()"
  const invalidValueParseBigIntArgBeforeFunc = "callerTestData.parseBigInt(callerTestData, callerTestData)"
  const invalidFunctionErrorResultParseBigInt = invalidFunctionError("parseBigInt", 1)

  const parseBigIntValue = "parseBigIntValue(callerTestData)"
  const parseBigIntValueArgBeforeFunc = "callerTestData.parseBigIntValue()"
  const invalidFunctionParseBigIntValue = "parseBigIntValue()"
  const invalidValueParseBigIntValueArgBeforeFunc = "callerTestData.parseBigIntValue(callerTestData,callerTestData)"

  const invalidFunctionErrorResultParseBigIntValue = invalidFunctionError("parseBigIntValue", 1)

    test("RIDE-62. ParseBigInt function should compile for valid values", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[randomStringArrayElement(), parseBigInt], [randomStringArrayElement(), parseBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-63. ParseBigInt function throws an error for invalid values", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func, error] of [[randomStringArrayElement(), parseBigInt, UNDEFINED_TYPE], [randomStringArrayElement(), parseBigIntArgBeforeFunc, UNDEFINED_TYPE]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-64. Function parseBigInt should throw an error for invalid functions", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func, error] of [[randomInt(), parseBigInt, nonMatchingTypes("String")], [randomBoolean(), parseBigIntArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), invalidFunctionParseBigInt, invalidFunctionErrorResultParseBigInt], [randomStringArrayElement(), invalidValueParseBigIntArgBeforeFunc, invalidFunctionErrorResultParseBigInt]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-65. ParseBigIntValue function should compile for valid values", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[randomStringArrayElement(), parseBigIntValue], [randomStringArrayElement(), parseBigIntValueArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-66. ParseBigIntValue function throws an error for invalid values", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func, error] of [[randomStringArrayElement(), parseBigIntValue, UNDEFINED_TYPE], [randomStringArrayElement(), parseBigIntValueArgBeforeFunc, UNDEFINED_TYPE]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-67. Function parseBigIntValue should throw an error for invalid functions", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func, error] of [[randomInt(), parseBigIntValue, nonMatchingTypes("String")], [randomBoolean(), parseBigIntValueArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), invalidFunctionParseBigIntValue, invalidFunctionErrorResultParseBigIntValue], [randomStringArrayElement(), invalidValueParseBigIntValueArgBeforeFunc, invalidFunctionErrorResultParseBigIntValue]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
