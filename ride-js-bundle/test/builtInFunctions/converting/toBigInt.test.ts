import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomBoolean, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ToBigInt", () => {
  const toBigInt = "toBigInt(callerTestData)"
  const toBigIntArgBeforeFunc = "callerTestData.toBigInt()"
  const toBigIntOnIndex = `toBigInt(callerTestData, 1, ${randomInt()})`
  const toBigIntOnIndexArgBeforeFunc = `callerTestData.toBigInt(9, ${randomInt()})`
  const invalidFunctionParseBigInt = "toBigInt()"
  const invalidParseBigIntArgBeforeFunc = `callerTestData.toBigInt(callerTestData, 123, ${randomInt()})`

    test("RIDE-72. ToBigInt function should compile for valid values", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func] of [[randomInt(), toBigInt], [randomByteVectorArrayElement(), toBigInt], [randomInt(), toBigIntArgBeforeFunc], [randomByteVectorArrayElement(), toBigIntArgBeforeFunc], [randomByteVectorArrayElement(), toBigIntOnIndex], [randomByteVectorArrayElement(), toBigIntOnIndexArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-73. ToBigInt function throws an error for invalid values", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func, error] of [[randomBoolean(), toBigIntArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomUnionArrayElement(), toBigIntArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomInt(), invalidFunctionParseBigInt, CANT_FIND_A_FUNCTION_OVERLOAD], [randomDigestAlgorithmTypeArrayElement(), toBigIntOnIndex, CANT_FIND_A_FUNCTION_OVERLOAD], [randomBoolean(), toBigIntOnIndexArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomInt(), toBigIntOnIndexArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomByteVectorArrayElement(), invalidFunctionParseBigInt, CANT_FIND_A_FUNCTION_OVERLOAD], [randomByteVectorArrayElement(), invalidParseBigIntArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
