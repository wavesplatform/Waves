import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { actualVersionsWithoutV3, intList, invalidFunctionError, nonMatchingTypes, stringList } from "../../helpers/testData";

describe("ContainsElement", () => {
  const containsElement = "containsElement(bar, foo)"
  const containsElementArgBeforeFunc = "bar.containsElement(foo)"
  const invalidContainsElement = "containsElement(foo)"
  const invalidContainsElementArgBeforeFunc = "foo.containsElement(foo, bar)"
  const invalidErrorContainsElement = invalidFunctionError("containsElement", 2)

    test("RIDE-154. Function ContainsElement should compile for valid list", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, containsElement], [randomInt(), intList, containsElement], [randomStringArrayElement(), stringList, containsElementArgBeforeFunc], [randomInt(), intList, containsElementArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-155. Function ContainsElement should throw an error for invalid data or type", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), containsElement, nonMatchingTypes("")], [randomStringArrayElement(), randomIssuesArrayElement(), containsElementArgBeforeFunc, nonMatchingTypes("")], [randomInt(), intList, invalidContainsElement, invalidErrorContainsElement], [randomStringArrayElement(), stringList, invalidContainsElementArgBeforeFunc, invalidErrorContainsElement]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
