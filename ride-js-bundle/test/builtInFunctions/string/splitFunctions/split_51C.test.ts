import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V5, V6 } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../../helpers/randomData";
import { CANT_FIND_FUNCTION, invalidFunctionError, nonMatchingTypes, versionsWithoutV6 } from "../../../helpers/testData";

describe("Split_51C", () => {
  const split_51C = `split_51C(bar, foo)`
  const split_51CArgBeforeFunc = `bar.split_51C(foo)`
  const invalidSplit_51C = `split_51C(foo)`
  const invalidErrorSplit_51C = invalidFunctionError("split_51C", 2)

    test("RIDE-205. split_51C function should compile for valid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func] of [[randomStringArrayElement(), randomStringArrayElement(), split_51C], [randomStringArrayElement(), randomStringArrayElement(), split_51CArgBeforeFunc]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileSuccessDApp(script, V6)
      }
  });

    test("RIDE-206. split_51C function should throw a compilation error for invalid data", () => {
      const precondition = new ContractGenerator("String", V6)
      for (const [data, list, func, error] of [[randomIssuesArrayElement(), randomStringArrayElement(), split_51C, nonMatchingTypes("String")], [randomInt(), randomStringArrayElement(), split_51CArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), randomStringArrayElement(), invalidSplit_51C, invalidErrorSplit_51C]]) {
        const script = precondition.simpleRideCode(data, list, func)
        assertCompileErrorDApp(script, V6, error)
      }
  });

    test("RIDE-207. Can't find a function split_4C for RIDE versions V3 - V5", () => {
      for (const version of versionsWithoutV6) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), randomStringArrayElement(), split_51C], [randomStringArrayElement(), randomStringArrayElement(), split_51CArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
