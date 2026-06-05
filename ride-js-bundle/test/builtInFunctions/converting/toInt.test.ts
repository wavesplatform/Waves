import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ToInt", () => {
  const toInt = "toInt(callerTestData)"
  const toIntArgBeforeFunc = "callerTestData.toInt()"
  const toIntOnIndex = `toInt(callerTestData, ${randomInt()})`
  const toIntOnIndexArgBeforeFunc = `callerTestData.toInt(${randomInt()})`
  const invalidFunctionParseInt = "toInt()"
  const invalidParseIntArgBeforeFunc = `callerTestData.toInt(callerTestData, ${randomInt()})`

    test("RIDE-77. Functions toInt function should compile for valid ByteVector", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), toInt], [randomByteVectorArrayElement(), toIntOnIndex], [randomByteVectorArrayElement(), toIntArgBeforeFunc], [randomByteVectorArrayElement(), toIntOnIndexArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-78. ToInt function should compile with bigInt for V5, V6 versions", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[`toBigInt(${randomInt()})`, toInt], [`toBigInt(${randomInt()})`, toIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-79. ToInt function throws an error for invalid values", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomDigestAlgorithmTypeArrayElement(), toInt], [randomStringArrayElement(), toIntOnIndex], [randomUnionArrayElement(), toIntOnIndexArgBeforeFunc], [randomInt(), toIntArgBeforeFunc], [`toBigInt(${randomInt()})`, invalidFunctionParseInt], [randomByteVectorArrayElement(), invalidParseIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
