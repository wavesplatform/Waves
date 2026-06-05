import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Pow", () => {
  const union = randomUnionArrayElement()
  const powInt = `pow(callerTestData, 6, ${randomInt()}, 4, ${randomInt()}, ${union})`
  const powIntArgBeforeFunc = `callerTestData.pow(6, ${randomInt()}, ${randomInt()}, 2, ${union})`
  const powBigInt = `pow(callerTestData, 6, callerTestData, 4, 2, ${union})`
  const powBigIntArgBeforeFunc = `callerTestData.pow(${randomInt()}, callerTestData, 4, 2, ${union})`
  const invalidPowInt = `pow()`
  const powError = invalidFunctionError("pow", 6)

    test("RIDE-185. Pow functions should compile with Int", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), powInt], [randomInt(), powIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-186. Pow functions should compile with BigInt for Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[`toBigInt(${randomInt()})`, powBigInt], [`toBigInt(${randomInt()})`, powBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-187. Median functions should throw an error for invalid Int data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func, error] of [[randomInt(), invalidPowInt, powError], [randomStringArrayElement(), powInt, nonMatchingTypes("Int")], [randomAddressDataArrayElement(), powIntArgBeforeFunc, nonMatchingTypes("Int")]]) {
          const script = precondition.onlyMatcherContract(data, func)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
  });

    test("RIDE-188. Median function should throw an error for invalid BigInt data - Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[randomStringArrayElement(), powBigInt], [randomAliasDataArrayElement(), powBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
