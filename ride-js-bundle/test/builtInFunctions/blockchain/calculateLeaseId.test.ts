import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4, V5 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomDigestAlgorithmTypeArrayElement, randomIssuesArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, CANT_FIND_FUNCTION, invalidFunctionError, oldVersions, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("CalculateLeaseId", () => {
  const calculateLeaseId = "calculateLeaseId(lease)"
  const calculateLeaseIdArgBeforeFunc = "lease.calculateLeaseId()"
  const invalidCalculateLeaseId = "calculateLeaseId()"

    test("RIDE-36. calculateLeaseId function should compile for version V5 and higher when called for an address", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), calculateLeaseId], [randomAddressDataArrayElement(), calculateLeaseIdArgBeforeFunc], [randomAliasDataArrayElement(), calculateLeaseId], [randomAliasDataArrayElement(), calculateLeaseIdArgBeforeFunc]]) {
          const script = precondition.codeForCalculateLeaseId(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-37. Negative cases for calculateLeaseId function for version V5 and higher", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func, error] of [[randomStringArrayElement(), calculateLeaseId, CANT_FIND_A_FUNCTION_OVERLOAD], [randomDigestAlgorithmTypeArrayElement(), calculateLeaseIdArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD], [randomAddressDataArrayElement(), invalidCalculateLeaseId, invalidFunctionError("calculateLeaseId", 1)], [randomAliasDataArrayElement(), invalidCalculateLeaseId, invalidFunctionError("calculateLeaseId", 1)]]) {
          const script = precondition.codeForCalculateLeaseId(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-38. Negative cases for calculateLeaseId function for versions V3 and V4", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomIssuesArrayElement(), calculateLeaseId], [randomIssuesArrayElement(), calculateLeaseIdArgBeforeFunc]]) {
          const script = precondition.codeForCalculateLeaseId(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
