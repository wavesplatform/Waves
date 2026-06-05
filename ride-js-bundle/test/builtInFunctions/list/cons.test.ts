import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomBoolean, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { actualVersions, intList, invalidFunctionError, nonMatchingTypes, stringList } from "../../helpers/testData";

describe("Cons", () => {
  const cons = "cons(foo, bar)"
  const consArgBeforeFunc = "foo.cons(bar)"
  const invalidCons = "cons(foo)"
  const invalidConsArgBeforeFunc = "foo.cons(foo, bar)"
  const invalidErrorCons = invalidFunctionError("cons", 2)

    test("RIDE-152. Function Cons should compile for valid list", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, cons], [randomInt(), intList, cons], [randomStringArrayElement(), stringList, consArgBeforeFunc], [randomInt(), intList, consArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-153. Function Cons should throw an error for invalid data or type", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), cons, nonMatchingTypes("")], [randomStringArrayElement(), randomBoolean(), consArgBeforeFunc, nonMatchingTypes("")], [randomInt(), intList, invalidCons, invalidErrorCons], [randomStringArrayElement(), stringList, invalidConsArgBeforeFunc, invalidErrorCons]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
