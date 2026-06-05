import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, intList, invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Min", () => {
  const min = "min(callerTestData)"
  const minArgBeforeFunc = "callerTestData.min()"
  const invalidMin = "min()"
  const invalidMinArgBeforeFunc = "callerTestData.min(callerTestData)"
  const minForBigInt = "min([callerTestData])"
  const minForBigIntArgBeforeFunc = "[callerTestData].min()"
  const invalidMinForBigInt = "[callerTestData].min([callerTestData], [callerTestData])"

    test("RIDE-165. Function Min should compile with a list", () => {
      for (const version of actualVersionsWithoutV3) {
        for (const [data, func, dataType] of [[intList, min, "Int"], [intList, minArgBeforeFunc, "Int"]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-166. Function Min should compile with a BigInt", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        for (const [data, func, dataType] of [[`toBigInt(${randomInt()})`, minForBigInt, "BigInt"], [`toBigInt(${randomInt()})`, minForBigIntArgBeforeFunc, "BigInt"]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-167. Function Min should throw an error for invalid data or type", () => {
      for (const version of actualVersionsWithoutV3) {
        for (const [data, func, dataType, error] of [[randomUnionArrayElement(), min, "Int", nonMatchingTypes("List[Int]")], [randomDigestAlgorithmTypeArrayElement(), minArgBeforeFunc, "Int", nonMatchingTypes("List[Int]")], [intList, invalidMin, "Int", invalidFunctionError("min", 1)], [intList, invalidMinArgBeforeFunc, "Int", invalidFunctionError("min", 1)], [intList, invalidMinForBigInt, "Int", invalidFunctionError("min", 1)]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          if (version === V4) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
  });
});
