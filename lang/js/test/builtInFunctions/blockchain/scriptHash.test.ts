import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4, V5 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, GreaterV3ResultBinaryEntry, invalidFunctionError, nonMatchingTypes, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ScriptHash", () => {
  const scriptHash = "scriptHash(callerTestData)"
  const scriptHashArgBeforeFunc = "callerTestData.scriptHash()"
  const invalidScriptHash = "scriptHash(callerTestData, callerTestData)"
  const invalidScriptHashArgBeforeFunc = `callerTestData.scriptHash(${randomStringArrayElement()})`

    test("RIDE-39. ScriptHash function should compile for version V5 and higher when called for an address", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), scriptHash], [randomAddressDataArrayElement(), scriptHashArgBeforeFunc], [randomAliasDataArrayElement(), scriptHash], [randomAliasDataArrayElement(), scriptHashArgBeforeFunc], [thisVariable, scriptHash], [thisVariable, scriptHashArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("Ride-40. Negative cases for ScriptHash function for version V5 and higher", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomStringArrayElement(), scriptHash, nonMatchingTypes("Address|Alias")], [randomDigestAlgorithmTypeArrayElement(), scriptHashArgBeforeFunc, nonMatchingTypes("Address|Alias")], [randomAddressDataArrayElement(), invalidScriptHash, invalidFunctionError("scriptHash", 1)], [randomAliasDataArrayElement(), invalidScriptHash, invalidFunctionError("scriptHash", 1)], [thisVariable, invalidScriptHashArgBeforeFunc, invalidFunctionError("scriptHash", 1)]]) {
          const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-41. Negative cases for ScriptHash function for versions V3 and V4", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), scriptHash], [randomAliasDataArrayElement(), scriptHashArgBeforeFunc]]) {
          const script = precondition.codeForCalculateLeaseId(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
