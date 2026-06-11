import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V5, V6 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, invalidFunctionError, nonMatchingTypes, stringList, versionsWithoutV6 } from "../../../helpers/testData";

describe("MakeString_11C", () => {
  const makeString_11C = `makeString_11C(bar, foo)`
  const makeString_11CArgBeforeFunc = `bar.makeString_11C(foo)`
  const invalidMakeString_11CFunction = `makeString_11C(foo)`
  const invalidErrorMakeString_11C = invalidFunctionError("makeString_11C", 2)

    test("RIDE-197. makeString_11C function should compile for valid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func] of [[randomStringArrayElement(), stringList, makeString_11C], [randomStringArrayElement(), stringList, makeString_11CArgBeforeFunc]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileSuccessDApp(script, V6)
      }
  });

    test("RIDE-198. makeString_11C function should throw a compilation error for invalid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func, error] of [[randomStringArrayElement(), stringList, invalidMakeString_11CFunction, invalidErrorMakeString_11C], [randomIssuesArrayElement(), stringList, makeString_11C, nonMatchingTypes("String")], [randomInt(), stringList, makeString_11CArgBeforeFunc, nonMatchingTypes("String")]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileErrorDApp(script, V6, error)
      }
  });

    test("RIDE-199. Can't find a function makeString_11C for RIDE versions V3 - V5", () => {
      for (const version of versionsWithoutV6) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, makeString_11C], [randomStringArrayElement(), stringList, makeString_11CArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
