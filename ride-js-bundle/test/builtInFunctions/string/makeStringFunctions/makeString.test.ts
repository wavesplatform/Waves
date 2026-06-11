import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../../helpers/randomData";
import { actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, stringList } from "../../../helpers/testData";

describe("MakeString", () => {
  const makeString = "makeString(bar, foo)"
  const makeStringArgBeforeFunc = "bar.makeString(foo)"
  const invalidMakeString = "makeString(foo)"
  const invalidMakeStringArgBeforeFunc = "foo.makeString(bar, foo)"
  const invalidErrorMakeString = invalidFunctionError("makeString", 2)

    test("RIDE-192. makeString function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, makeString], [randomStringArrayElement(), stringList, makeStringArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-193. makeString function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), makeStringArgBeforeFunc, nonMatchingTypes("List[String]")], [randomInt(), randomIssuesArrayElement(), makeStringArgBeforeFunc, nonMatchingTypes("List[String]")], [randomStringArrayElement(), stringList, invalidMakeString, invalidErrorMakeString], [randomStringArrayElement(), stringList, invalidMakeStringArgBeforeFunc, invalidErrorMakeString]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
