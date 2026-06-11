import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBooleanEntry, actualVersions, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("GetBoolean", () => {
  // getBoolean
  const getBoolean = `getBoolean(callerTestData, ${randomStringArrayElement()})`
  const getBooleanArgBeforeFunc = `callerTestData.getBoolean(${randomStringArrayElement()})`
  const ownDataGetBoolean = `getBoolean(${randomStringArrayElement()})`
  const ownDataGetBooleanArgBeforeFunc = `${randomStringArrayElement()}.getBoolean()`

  // getBooleanValue
  const getBooleanValue = `getBooleanValue(callerTestData, ${randomStringArrayElement()})`
  const getBooleanValueArgBeforeFunc = `callerTestData.getBooleanValue(${randomStringArrayElement()})`
  const ownDataGetBooleanValue = `getBooleanValue(${randomStringArrayElement()})`
  const ownDataGetBooleanValueArgBeforeFunc = `${randomStringArrayElement()}.getBooleanValue()`

  const invalidGetBoolean = `getBoolean(callerTestData)`
  const invalidGetBooleanValue = `getBooleanValue(callerTestData)`

    test("RIDE-8. Compile getBoolean functions for address, alias, and 'this'", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, booleanData] of [[randomAddressDataArrayElement(), getBoolean], [randomAddressDataArrayElement(), getBooleanArgBeforeFunc], [randomAliasDataArrayElement(), getBoolean], [randomAliasDataArrayElement(), getBooleanArgBeforeFunc], [randomAddressDataArrayElement(), getBooleanValue], [randomAddressDataArrayElement(), getBooleanValueArgBeforeFunc], [randomAliasDataArrayElement(), getBooleanValue], [randomAliasDataArrayElement(), getBooleanValueArgBeforeFunc], [thisVariable, getBoolean], [thisVariable, getBooleanArgBeforeFunc], [thisVariable, getBooleanValue], [thisVariable, getBooleanValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, booleanData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-9. Compile own data getBoolean functions for address, alias, and 'this'", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const ownData of [ownDataGetBoolean, ownDataGetBooleanArgBeforeFunc, ownDataGetBooleanValueArgBeforeFunc, ownDataGetBooleanValue]) {
          const script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-10. Negative tests for getBoolean functions", () => {
      const invalidFunction = `getBooleanValue(${randomInt()})`
      const invalidArgBeforeFunction = `${randomInt()}.getBooleanValue()`
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, booleanData] of [[randomAddressDataArrayElement(), invalidGetBoolean], [randomAliasDataArrayElement(), invalidGetBooleanValue], [randomInt(), getBoolean], [randomInt(), getBooleanValue], [randomInt(), invalidFunction], [randomInt(), invalidArgBeforeFunction]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, booleanData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-11. Ensure no overload of own data Boolean accountDataStorage for old Versions", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [addressOrAlias, booleanData] of [[randomAddressDataArrayElement(), ownDataGetBooleanValue], [randomAliasDataArrayElement(), ownDataGetBooleanValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, booleanData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
