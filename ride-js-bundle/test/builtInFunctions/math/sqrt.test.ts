import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V5, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, CANT_FIND_FUNCTION, versionsWithoutV6 } from "../../helpers/testData";

describe("Sqrt", () => {
  const union = randomUnionArrayElement()
  const sqrtIntAndUnion = `sqrt(callerTestData, ${randomInt()}, ${randomInt()}, ${union})`
  const sqrtIntAndUnionArgBeforeFunc = `callerTestData.sqrt(${randomInt()}, 3, ${union})`
  const invalidSqrtFunction = `sqrt(callerTestData)`

    test("RIDE-189. Sqrt functions compile with Int and BigInt", () => {
      for (const [data, func, dataType] of [[randomInt(), sqrtIntAndUnion, "Int"], [randomInt(), sqrtIntAndUnionArgBeforeFunc, "Int"], [`toBigInt(${randomInt()})`, sqrtIntAndUnion, "BigInt"], [`toBigInt(${randomInt()})`, sqrtIntAndUnionArgBeforeFunc, "BigInt"]]) {
        const precondition = new ContractGenerator(dataType, V6)
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileSuccessDApp(script, V6)
      }
  });

    test("RIDE-190. Sqrt functions should throw an error for invalid Int data", () => {
      for (const [data, func, dataType] of [[randomStringArrayElement(), sqrtIntAndUnion, "Int"], [randomInt(), invalidSqrtFunction, "Int"], [randomByteVectorArrayElement(), sqrtIntAndUnion, "BigInt"], [`toBigInt(${randomInt()})`, invalidSqrtFunction, "BigInt"]]) {
        const precondition = new ContractGenerator(dataType, V6)
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileErrorDApp(script, V6, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });

    test("RIDE-191. Can't find a function Sqrt for V3 - V5", () => {
      for (const version of versionsWithoutV6) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), sqrtIntAndUnion], [randomInt(), sqrtIntAndUnionArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
