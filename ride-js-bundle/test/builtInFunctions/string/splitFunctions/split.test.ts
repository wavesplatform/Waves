import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../../helpers/jsTestBase";
import { ContractGenerator } from "../../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../../helpers/testData";

describe("Split", () => {
  const split = "split(bar, foo)"
  const splitArgBeforeFunc = "bar.split(foo)"
  const invalidSplit = "split(foo)"
  const invalidSplitArgBeforeFunc = "foo.split(bar, foo)"
  const invalidErrorSplit = invalidFunctionError("split", 2)

    test("RIDE-200. split function should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, list, func] of [[randomStringArrayElement(), randomStringArrayElement(), split], [randomStringArrayElement(), randomStringArrayElement(), splitArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-201. split function should throw a compilation error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func, error] of [[randomInt(), randomAliasDataArrayElement(), splitArgBeforeFunc, nonMatchingTypes("String")], [randomInt(), randomIssuesArrayElement(), splitArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), randomStringArrayElement(), invalidSplit, invalidErrorSplit], [randomStringArrayElement(), randomStringArrayElement(), invalidSplitArgBeforeFunc, invalidErrorSplit]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
