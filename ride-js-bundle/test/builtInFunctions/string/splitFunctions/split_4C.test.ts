import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V5, V6 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, invalidFunctionError, nonMatchingTypes, versionsWithoutV6 } from "../../../helpers/testData";

describe("Split_4C", () => {
  const split_4C = `split_4C(bar, foo)`
  const split_4CArgBeforeFunc = `bar.split_4C(foo)`
  const invalidSplit_4C = `split_4C(foo)`
  const invalidErrorSplit_4C = invalidFunctionError("split_4C", 2)

    test("RIDE-202. split_4C function should compile for valid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func] of [[randomStringArrayElement(), randomStringArrayElement(), split_4C], [randomStringArrayElement(), randomStringArrayElement(), split_4CArgBeforeFunc]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileSuccessDApp(script, V6)
      }
  });

    test("RIDE-203. split_4C function should throw a compilation error for invalid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func, error] of [[randomIssuesArrayElement(), randomStringArrayElement(), split_4C, nonMatchingTypes("String")], [randomInt(), randomStringArrayElement(), split_4CArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), randomStringArrayElement(), invalidSplit_4C, invalidErrorSplit_4C]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileErrorDApp(script, V6, error)
      }
  });

    test("RIDE-204. Can't find a function split_4C for RIDE versions V3 - V5", () => {
      for (const version of versionsWithoutV6) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), randomStringArrayElement(), split_4C], [randomStringArrayElement(), randomStringArrayElement(), split_4CArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
