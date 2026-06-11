import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomInt } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("AssetInfo", () => {
  const assetInfo = "assetInfo(callerTestData)"
  const assetInfoArgBeforeFunc = "callerTestData.assetInfo()"

  const invalidAssetInfo = "assetInfo()"
  const invalidAssetInfoArg = `${randomInt()}.assetInfo()`


    test("RIDE-30. Compile assetInfo function for asset", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Asset", version)
        for (const [asset, func] of [[randomByteVectorArrayElement(), assetInfo], [randomByteVectorArrayElement(), assetInfoArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(asset, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-29. Invalid data must be validated", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Asset", version)
        for (const [asset, func, error] of [[randomByteVectorArrayElement(), invalidAssetInfo, invalidFunctionError("assetInfo", 1)], [randomByteVectorArrayElement(), invalidAssetInfoArg, nonMatchingTypes("ByteVector")], [randomAliasDataArrayElement(), assetInfo, nonMatchingTypes("ByteVector")], [randomBoolean(), assetInfoArgBeforeFunc, nonMatchingTypes("ByteVector")]]) {
          const script = precondition.onlyMatcherContract(asset, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
