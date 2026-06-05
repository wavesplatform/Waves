import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomBoolean, randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ToString", () => {
  const toStr = "toString(callerTestData)"
  const toStrArgBeforeFunc = "callerTestData.toString()"
  const invalidToStr = "toString()"
  const invalidToStrArgBeforeFunc = "callerTestData.toString(callerTestData)"

    test("RIDE-80. Functions toString function should compile with int, string, boolean", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), toStr], [randomInt(), toStr], [randomBoolean(), toStr], [randomAddressDataArrayElement(), toStrArgBeforeFunc], [randomInt(), toStrArgBeforeFunc], [randomBoolean(), toStrArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-81. Functions toString should compile with bigInt for V5, V6 versions", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func] of [[`toBigInt(${randomInt()})`, toStr], [`toBigInt(${randomInt()})`, toStrArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-82. toString function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), toStr, CANT_FIND_A_FUNCTION_OVERLOAD], [randomDigestAlgorithmTypeArrayElement(), toStrArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomInt(), invalidToStr, CANT_FIND_A_FUNCTION_OVERLOAD], [randomAddressDataArrayElement(), invalidToStrArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
