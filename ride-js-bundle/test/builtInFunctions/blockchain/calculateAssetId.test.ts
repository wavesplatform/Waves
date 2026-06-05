import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomDigestAlgorithmTypeArrayElement, randomIssuesArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("CalculateAssetId", () => {
  const calculateAssetId = "calculateAssetId(issue)"
  const calculateAssetIdArgBeforeFunc = "issue.calculateAssetId()"
  const invalidCalculateAssetId = "calculateAssetId()"

    test("RIDE-33. CalculateAssetId function should compile for version V4 and higher when called for Issue operatio", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomIssuesArrayElement(), calculateAssetId], [randomIssuesArrayElement(), calculateAssetIdArgBeforeFunc]]) {
          const script = precondition.codeForCalculateAssetId(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-34. Negative cases for CalculateAssetId function for version V4 and higher", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func, error] of [[randomDigestAlgorithmTypeArrayElement(), calculateAssetId, nonMatchingTypes("Issue")], [randomDigestAlgorithmTypeArrayElement(), calculateAssetIdArgBeforeFunc, nonMatchingTypes("Issue")], [randomDigestAlgorithmTypeArrayElement(), invalidCalculateAssetId, invalidFunctionError("calculateAssetId", 1)]]) {
          const script = precondition.codeForCalculateAssetId(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-35. Negative cases for CalculateAssetId function for version V3", () => {
        const precondition = new ContractGenerator("", V3)
        for (const [data, func] of [[randomIssuesArrayElement(), calculateAssetId], [randomIssuesArrayElement(), calculateAssetIdArgBeforeFunc]]) {
          const script = precondition.codeForCalculateAssetId(data, func)
          assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
  });
});
