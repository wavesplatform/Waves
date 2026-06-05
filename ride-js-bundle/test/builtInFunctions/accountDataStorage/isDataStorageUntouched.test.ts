import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, GreaterV3ResultBooleanEntry, invalidFunctionError, nonMatchingTypes, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("IsDataStorageUntouched", () => {
  const isDataStorageUntouched = "isDataStorageUntouched(callerTestData)"
  const isDataStorageUntouchedArgBeforeFunc = "callerTestData.isDataStorageUntouched()"
  const invalidFunction = "isDataStorageUntouched()"
  const invalidFunctionErrorResult = invalidFunctionError("isDataStorageUntouched", 1)

    test("RIDE-20. Compile isDataStorageUntouched functions for address, alias, and 'this'", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, dataStorage] of [[randomAddressDataArrayElement(), isDataStorageUntouched], [randomAddressDataArrayElement(), isDataStorageUntouchedArgBeforeFunc], [randomAliasDataArrayElement(), isDataStorageUntouched], [randomAliasDataArrayElement(), isDataStorageUntouchedArgBeforeFunc], [thisVariable, isDataStorageUntouched], [thisVariable, isDataStorageUntouchedArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-21. Non-matching types for function isDataStorageUntouched", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, dataStorage] of [[randomInt(), isDataStorageUntouched], [randomInt(), isDataStorageUntouchedArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, nonMatchingTypes("Address|Alias"))
        }
      }
  });

    test("RIDE-22. Invalid data for functions isDataStorageUntouched", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, dataStorage] of [[randomAddressDataArrayElement(), invalidFunction], [randomAliasDataArrayElement(), invalidFunction], [thisVariable, invalidFunction]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
        }
      }
  });

    test("RIDE-23. Can't find functions isDataStorageUntouched dataStorage accountDataStorage for old Versions", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, dataStorage] of [[randomAddressDataArrayElement(), isDataStorageUntouchedArgBeforeFunc], [thisVariable, isDataStorageUntouchedArgBeforeFunc], [randomAliasDataArrayElement(), isDataStorageUntouched]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
