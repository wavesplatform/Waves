import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { actualVersions, intList, invalidFunctionError, nonMatchingTypes, stringList } from "../../helpers/testData";

describe("GetElement", () => {
  const getElement = "getElement(bar, foo)"
  const getElementArgBeforeFunc = "bar.getElement(foo)"
  const invalidGetElement = "getElement(foo)"
  const invalidGetElementArgBeforeFunc = "foo.getElement(bar, foo)"
  const invalidErrorGetElement = invalidFunctionError("getElement", 2)

    test("RIDE-156. Function GetElement should compile for valid list", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[randomInt(), stringList, getElement], [randomInt(), intList, getElement], [randomInt(), stringList, getElementArgBeforeFunc], [randomInt(), intList, getElementArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-157. Function GetElement should throw an error for invalid data or type", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), getElement, nonMatchingTypes("")], [randomStringArrayElement(), randomIssuesArrayElement(), getElementArgBeforeFunc, nonMatchingTypes("")], [randomInt(), intList, invalidGetElement, invalidErrorGetElement], [randomStringArrayElement(), stringList, invalidGetElementArgBeforeFunc, invalidErrorGetElement]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
