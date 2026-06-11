import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, invalidFunctionError, nonMatchingTypes, oldVersions, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Fraction", () => {
  let union = randomUnionArrayElement()
  const fractionInt = `fraction(callerTestData, ${randomInt()}, ${randomInt()})`
  const fractionIntArgBeforeFunc = `callerTestData.fraction(${randomInt()}, ${randomInt()})`
  const fractionIntAndUnion = `fraction(callerTestData, ${randomInt()}, ${randomInt()}, ${union})`
  const fractionIntAndUnionArgBeforeFunc = `callerTestData.fraction(${randomInt()}, ${randomInt()}, ${union})`

  union = randomUnionArrayElement()
  const fractionBigInt = `fraction(callerTestData, callerTestData, callerTestData)`
  const fractionBigIntArgBeforeFunc = `callerTestData.fraction(callerTestData, callerTestData)`
  const fractionBigIntAndUnion = `fraction(callerTestData, callerTestData, callerTestData, ${union})`
  const fractionBigIntAndUnionArgBeforeFunc = `callerTestData.fraction(callerTestData, callerTestData, ${union})`

  const invalidFractionFunction = `fraction(callerTestData)`
  const invalidFractionFunctionArgBeforeFunc = `callerTestData.fraction()`
  const fractionError = invalidFunctionError("fraction", 3)

    test("RIDE-172. Fraction should compile with the Int type", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), fractionInt], [randomInt(), fractionIntArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-173. Fraction should compile with the Int and Union types - Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        for (const [data, func, dataType] of [[randomInt(), fractionIntAndUnion, "Int"], [randomInt(), fractionIntAndUnionArgBeforeFunc, "Int"], [`toBigInt(${randomInt()})`, fractionBigInt, "BigInt"], [`toBigInt(${randomInt()})`, fractionBigIntArgBeforeFunc, "BigInt"], [`toBigInt(${randomInt()})`, fractionBigIntAndUnion, "BigInt"], [`toBigInt(${randomInt()})`, fractionBigIntAndUnionArgBeforeFunc, "BigInt"]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-174. Fraction should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        for (const [data, func, dataType, error] of [[randomInt(), invalidFractionFunction, "Int", fractionError], [randomInt(), invalidFractionFunctionArgBeforeFunc, "Int", fractionError], [randomStringArrayElement(), fractionInt, "Int", nonMatchingTypes("Int")], [randomAddressDataArrayElement(), fractionIntArgBeforeFunc, "Int", nonMatchingTypes("Int")]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
  });

    test("RIDE-175. Fraction should raise a compilation error for BigInt - Ride V5, V6", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        for (const [data, func, dataType, error] of [[randomStringArrayElement(), fractionBigInt, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD], [randomAliasDataArrayElement(), fractionBigIntArgBeforeFunc, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD], [randomStringArrayElement(), fractionBigIntAndUnion, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD], [randomAliasDataArrayElement(), fractionBigIntAndUnionArgBeforeFunc, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD], [randomIssuesArrayElement(), fractionIntAndUnion, "Int", CANT_FIND_A_FUNCTION_OVERLOAD], [randomIssuesArrayElement(), fractionIntAndUnionArgBeforeFunc, "Int", CANT_FIND_A_FUNCTION_OVERLOAD]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-176. Fraction should raise an error for versions V3 and V4 with incorrect argument count", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("Int", version)
        const script = precondition.onlyMatcherContract(randomInt(), fractionIntAndUnion)
        assertCompileErrorDApp(script, version, fractionError)
      }
  });
});
