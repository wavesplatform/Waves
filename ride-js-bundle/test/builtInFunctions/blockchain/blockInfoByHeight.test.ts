import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomBoolean, randomInt } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("BlockInfoByHeight", () => {
  const blockInfoByHeight = "blockInfoByHeight(callerTestData)"
  const blockInfoByHeightArgBeforeFunc = "callerTestData.blockInfoByHeight()"

  const invalidBlockInfoByHeight = "blockInfoByHeight()"

    test("RIDE-31. BlockInfoByHeight function should compile", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("BlockInfo", version)
        for (const [intData, func] of [[randomInt(), blockInfoByHeight], [randomInt(), blockInfoByHeightArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(intData, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-32. Negative cases for blockInfoByHeight function when invalid arguments are passed", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("BlockInfo", version)
        for (const [intData, func, error] of [[randomInt(), invalidBlockInfoByHeight, invalidFunctionError("blockInfoByHeight", 1)], [randomAliasDataArrayElement(), blockInfoByHeight, nonMatchingTypes("Int")], [randomBoolean(), blockInfoByHeightArgBeforeFunc, nonMatchingTypes("Int")]]) {
          const script = precondition.onlyMatcherContract(intData, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
