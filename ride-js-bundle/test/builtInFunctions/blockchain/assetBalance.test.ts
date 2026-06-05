import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { GreaterV3ResultIntegerEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result, thisVariable } from "../../helpers/testData";

describe("AssetBalance", () => {
  const address = randomAddressDataArrayElement()
  const alias = randomAliasDataArrayElement()
  const byteVector = randomByteVectorArrayElement()

  const assetBalance = `assetBalance(callerTestData, ${byteVector})`
  const assetBalanceArgBeforeFunc = `callerTestData.assetBalance(${byteVector})`
  const invalidAssetBalanceFunc = "assetBalance()"

    test("RIDE-27. Compile assetBalance function for address, alias, and 'this'", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [addressOrAlias, func] of [[address, assetBalance], [alias, assetBalance], [thisVariable, assetBalance], [address, assetBalanceArgBeforeFunc], [alias, assetBalanceArgBeforeFunc], [thisVariable, assetBalanceArgBeforeFunc]]) {
          const script = precondition.codeWithoutMatcher(addressOrAlias, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-28. Invalid data must be validated", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [addressOrAlias, func, error] of [[address, invalidAssetBalanceFunc, invalidFunctionError("assetBalance", 2)], [alias, invalidAssetBalanceFunc, invalidFunctionError("assetBalance", 2)], [thisVariable, invalidAssetBalanceFunc, invalidFunctionError("assetBalance", 2)], [randomUnionArrayElement(), assetBalance, nonMatchingTypes("Address|Alias")], [randomByteVectorArrayElement(), assetBalanceArgBeforeFunc, nonMatchingTypes("Address|Alias")]]) {
          const script = precondition.codeWithoutMatcher(addressOrAlias, func, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
