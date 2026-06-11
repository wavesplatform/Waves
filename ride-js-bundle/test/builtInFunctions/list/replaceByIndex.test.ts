import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, allStdLibVersions, V8 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { nonMatchingTypes, invalidFunctionError, stringList } from "../../helpers/testData";

describe("ReplaceByIndex", () => {
  const replaceByIndex = `replaceByIndex(bar, 1, foo)`;
  const replaceByIndexArgBeforeFunc = `bar.replaceByIndex(1, foo)`;
  const invalidReplaceByIndex = `replaceByIndex(1, foo)`;
  const invalidReplaceByIndexArgBeforeFunc = `foo.replaceByIndex(bar, 1, foo)`;
  const invalidErrorReplaceByIndex = invalidFunctionError("replaceByIndex", 3);

  test("replaceByIndex functions compiles with a list", () => {
    for (const version of allStdLibVersions.filter((v) => v >= V8)) {
      const precondition = new ContractGenerator("", version);
      for (const [data, list, func] of [
        [randomStringArrayElement(), stringList, replaceByIndex],
        [randomStringArrayElement(), stringList, replaceByIndexArgBeforeFunc]
      ]) {
        const script = precondition.simpleRideCode(data, list, func);
        assertCompileSuccessDApp(script, version);
      }
    }
  });

  test("Compilation errors ReplaceByIndex functions", () => {
    for (const version of allStdLibVersions.filter((v) => v >= V8)) {
      const precondition = new ContractGenerator("", version);
      for (const [data, list, func, error] of [
        [randomInt(), randomAliasDataArrayElement(), replaceByIndex, nonMatchingTypes("List[T]")],
        [randomInt(), randomIssuesArrayElement(), replaceByIndexArgBeforeFunc, nonMatchingTypes("List[T]")],
        [randomInt(), stringList, invalidReplaceByIndex, invalidErrorReplaceByIndex],
        [randomInt(), stringList, invalidReplaceByIndexArgBeforeFunc, invalidErrorReplaceByIndex]
      ]) {
        const script = precondition.simpleRideCode(data, list, func);
        assertCompileErrorDApp(script, version, error);
      }
    }
  });
});
