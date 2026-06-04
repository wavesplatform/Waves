import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement } from "../../helpers/randomData";
import { actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, stringList } from "../../helpers/testData";

describe("RemoveByIndex", () => {
  const removeByIndex = "removeByIndex(bar, foo)"
  const removeByIndexArgBeforeFunc = "bar.removeByIndex(foo)"
  const invalidRemoveByIndex = "removeByIndex(foo)"
  const invalidRemoveByIndexArgBeforeFunc = "foo.removeByIndex(bar, foo)"
  const invalidErrorRemoveByIndex = invalidFunctionError("removeByIndex", 2)

    test("RIDE-168. Function RemoveByIndex should compile for valid list", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[randomInt(), stringList, removeByIndex], [randomInt(), stringList, removeByIndexArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-169. Function RemoveByIndex should throw an error for invalid data or type", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), removeByIndex, nonMatchingTypes("List[T]")], [randomInt(), randomIssuesArrayElement(), removeByIndexArgBeforeFunc, nonMatchingTypes("List[T]")], [randomInt(), stringList, invalidRemoveByIndex, invalidErrorRemoveByIndex], [randomInt(), stringList, invalidRemoveByIndexArgBeforeFunc, invalidErrorRemoveByIndex]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
