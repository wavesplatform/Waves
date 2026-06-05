import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V5, V6 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, invalidFunctionError, nonMatchingTypes, stringList, versionsWithoutV6 } from "../../../helpers/testData";

describe("MakeString_2C", () => {
  const makeString_2C = `makeString_2C(bar, foo)`
  const makeString_2CArgBeforeFunc = `bar.makeString_2C(foo)`
  const invalidMakeString_2CFunction = `makeString_2C(foo)`
  const invalidErrorMakeString_2C = invalidFunctionError("makeString_2C", 2)

    test("RIDE-194. makeString_2C function should compile for valid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func] of [[randomStringArrayElement(), stringList, makeString_2C], [randomStringArrayElement(), stringList, makeString_2CArgBeforeFunc]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileSuccessDApp(script, V6)
      }
  });

    test("RIDE-195. makeString_2C function should throw a compilation error for invalid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func, error] of [[randomStringArrayElement(), stringList, invalidMakeString_2CFunction, invalidErrorMakeString_2C], [randomIssuesArrayElement(), stringList, makeString_2C, nonMatchingTypes("String")], [randomInt(), stringList, makeString_2CArgBeforeFunc, nonMatchingTypes("String")]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileErrorDApp(script, V6, error)
      }
  });

    test("RIDE-196. Can't find a function makeString_2C for RIDE versions V3 - V5", () => {
      for (const version of versionsWithoutV6) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, makeString_2C], [randomStringArrayElement(), stringList, makeString_2CArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
