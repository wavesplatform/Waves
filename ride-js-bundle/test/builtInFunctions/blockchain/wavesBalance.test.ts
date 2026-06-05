import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, thisVariable } from "../../helpers/testData";

describe("WavesBalance", () => {
  const wavesBalance = "wavesBalance(callerTestData)"
  const wavesBalanceArgBeforeFunc = "callerTestData.wavesBalance()"
  const invalidWavesBalance = "wavesBalance()"
  const invalidWavesBalanceArg = `callerTestData.wavesBalance(callerTestData)`

    test("RIDE-46. wavesBalance function for version V4 and more should compile for address, alias, and 'this'", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("BalanceDetails", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), wavesBalance], [randomAliasDataArrayElement(), wavesBalance], [thisVariable, wavesBalance], [randomAddressDataArrayElement(), wavesBalanceArgBeforeFunc], [randomAliasDataArrayElement(), wavesBalanceArgBeforeFunc], [thisVariable, wavesBalanceArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-47. Negative cases for wavesBalance function for version V4 and more", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("BalanceDetails", version)
        for (const [data, func, error] of [[randomDigestAlgorithmTypeArrayElement(), wavesBalance, nonMatchingTypes("Address|Alias")], [randomStringArrayElement(), wavesBalanceArgBeforeFunc, nonMatchingTypes("Address|Alias")], [randomAddressDataArrayElement(), invalidWavesBalanceArg, invalidFunctionError("wavesBalance", 1)], [randomAliasDataArrayElement(), invalidWavesBalance, invalidFunctionError("wavesBalance", 1)]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-48. Functions wavesBalance for V3 compiles for address, alias and 'this'", () => {
        const precondition = new ContractGenerator("Int", V3)
        for (const [data, func] of [[randomAddressDataArrayElement(), wavesBalance], [randomAliasDataArrayElement(), wavesBalance], [thisVariable, wavesBalance], [randomAddressDataArrayElement(), wavesBalanceArgBeforeFunc], [randomAliasDataArrayElement(), wavesBalanceArgBeforeFunc], [thisVariable, wavesBalanceArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-49. compilation error: wavesBalance for V3 Non-matching type", () => {
      const precondition = new ContractGenerator("Int", V3)
      for (const [data, func, error] of [[randomDigestAlgorithmTypeArrayElement(), wavesBalance, nonMatchingTypes("Address|Alias")], [randomStringArrayElement(), wavesBalanceArgBeforeFunc, nonMatchingTypes("Address|Alias")], [randomAddressDataArrayElement(), invalidWavesBalanceArg, invalidFunctionError("wavesBalance", 1)], [randomAliasDataArrayElement(), invalidWavesBalance, invalidFunctionError("wavesBalance", 1)]]) {
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileErrorDApp(script, V3, error)
      }
  });
});
