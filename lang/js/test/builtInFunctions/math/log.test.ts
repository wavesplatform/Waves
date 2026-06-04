import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Log", () => {
  const union = randomUnionArrayElement()
  const logInt = `log(callerTestData, ${randomInt()}, ${randomInt()}, 4, 2, ${union})`
  const logIntArgBeforeFunc = `callerTestData.log(${randomInt()}, ${randomInt()}, 4, 2, ${union})`
  const logBigInt = `log(callerTestData, 6, callerTestData, ${randomInt()}, 2, ${union})`
  const logBigIntArgBeforeFunc = `callerTestData.log(6, callerTestData, ${randomInt()}, 2, ${union})`
  const invalidLogInt = `log(callerTestData, 10, ${union})`
  const invalidLogIntArgBeforeFunc = `callerTestData.log(10, ${union})`
  const logError = invalidFunctionError("log", 6)

    test("RIDE-177. Log function should compile with Int", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), logInt], [randomInt(), logIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-178. Log function should compile with BigInt for Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[`toBigInt(${randomInt()})`, logBigInt], [`toBigInt(${randomInt()})`, logBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-179. Log function should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func, error] of [[randomStringArrayElement(), logInt, nonMatchingTypes("Int")], [randomAddressDataArrayElement(), logIntArgBeforeFunc, nonMatchingTypes("Int")], [randomInt(), invalidLogInt, logError], [randomInt(), invalidLogIntArgBeforeFunc, logError]]) {
          const script = precondition.onlyMatcherContract(data, func)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
  });

    test("RIDE-180. Log function should throw an error for invalid data BigInt - Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[randomStringArrayElement(), logBigInt], [randomAliasDataArrayElement(), logBigIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
