import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, intList, invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Max", () => {
  const max = "max(callerTestData)"
  const maxArgBeforeFunc = "callerTestData.max()"
  const invalidMax = "max()"
  const invalidMaxArgBeforeFunc = "callerTestData.max(callerTestData)"
  const maxForBigInt = "max([callerTestData])"
  const maxForBigIntArgBeforeFunc = "[callerTestData].max()"
  const invalidMaxForBigInt = "[callerTestData].max([callerTestData], [callerTestData])"

    test("RIDE-162. Function Max should compile with a list", () => {
      for (const version of actualVersionsWithoutV3) {
        for (const [data, func, dataType] of [[intList, max, "Int"], [intList, maxArgBeforeFunc, "Int"]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-163. Function Max should compile with a BigInt", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        for (const [data, func, dataType] of [[`toBigInt(${randomInt()})`, maxForBigInt, "BigInt"], [`toBigInt(${randomInt()})`, maxForBigIntArgBeforeFunc, "BigInt"]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-164. Function Max should throw an error for invalid data or type", () => {
      for (const version of actualVersionsWithoutV3) {
        for (const [data, func, dataType, error] of [[randomUnionArrayElement(), max, "Int", nonMatchingTypes("List[Int]")], [randomDigestAlgorithmTypeArrayElement(), maxArgBeforeFunc, "Int", nonMatchingTypes("List[Int]")], [intList, invalidMax, "Int", invalidFunctionError("max", 1)], [intList, invalidMaxArgBeforeFunc, "Int", invalidFunctionError("max", 1)], [intList, invalidMaxForBigInt, "Int", invalidFunctionError("max", 1)]]) {
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
