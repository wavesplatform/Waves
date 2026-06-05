import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("Contains", () => {
  const contains = "contains(bar, foo)"
  const containsArgBeforeFunc = "bar.contains(foo)"
  const invalidContains = "contains(foo)"
  const invalidContainsArgBeforeFunc = "foo.contains(bar, foo)"
  const invalidErrorContains = invalidFunctionError("contains", 2)

    test("RIDE-208. function contains should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), randomStringArrayElement(), contains], [randomStringArrayElement(), randomStringArrayElement(), containsArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-209. function contains throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), containsArgBeforeFunc, nonMatchingTypes("String")], [randomInt(), randomIssuesArrayElement(), containsArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), randomStringArrayElement(), invalidContains, invalidErrorContains], [randomStringArrayElement(), randomStringArrayElement(), invalidContainsArgBeforeFunc, invalidErrorContains]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }

      }
  });

    test("RIDE-210. Can't find a function contains for RIDE V3", () => {
      const precondition = new ContractGenerator("String", V3)
      const script = precondition.simpleRideCode(randomStringArrayElement(), randomStringArrayElement(), contains)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
