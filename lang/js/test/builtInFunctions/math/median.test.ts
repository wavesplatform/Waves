import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Median", () => {
  const medianInt = `median([callerTestData, ${randomInt()}, ${randomInt()}])`
  const medianIntArgBeforeFunc = `[callerTestData, ${randomInt()}, ${randomInt()}].median()`
  const medianBigInt = `median(callerTestData)`
  const medianBigIntArgBeforeFunc = `callerTestData.median()`
  const invalidMedianInt = `median()`
  const medianError = invalidFunctionError("median", 1)

    test("RIDE-181. Median functions should compile with Int", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), medianInt], [randomInt(), medianIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-182. Median functions should compile with BigInt for Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[`[toBigInt(${randomInt()})]`, medianBigInt], [`[toBigInt(${randomInt()})]`, medianBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-183. Median functions should throw an error for invalid Int data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func, error] of [[randomInt(), invalidMedianInt, medianError], [randomStringArrayElement(), medianInt, nonMatchingTypes("List[Int]")], [randomAddressDataArrayElement(), medianIntArgBeforeFunc, nonMatchingTypes("List[Int]")]]) {
          const script = precondition.onlyMatcherContract(data, func)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
  });

    test("RIDE-184. Median function should throw an error for invalid BigInt data - Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[randomStringArrayElement(), medianBigInt], [randomAliasDataArrayElement(), medianBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
