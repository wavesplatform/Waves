import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("GetString", () => {
  // getString
  const getString = `getString(callerTestData, ${randomStringArrayElement()})`
  const getStringArgBeforeFunc = `callerTestData.getString(${randomStringArrayElement()})`
  const ownDataGetString = `getString(${randomStringArrayElement()})`
  const ownDataGetStringArgBeforeFunc = `${randomStringArrayElement()}.getString()`

  // getStringValue
  const getStringValue = `getStringValue(callerTestData, ${randomStringArrayElement()})`
  const getStringValueArgBeforeFunc = `callerTestData.getStringValue(${randomStringArrayElement()})`
  const ownDataGetStringValue = `getStringValue(${randomStringArrayElement()})`
  const ownDataGetStringValueArgBeforeFunc = `${randomStringArrayElement()}.getStringValue()`

  const invalidGetString = `getString(callerTestData)`
  const invalidGetStringValue = `getStringValue(callerTestData)`

    test("RIDE-16. Compile getString functions for address, alias, and 'this'", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [addressOrAlias, stringData] of [[randomAddressDataArrayElement(), getString], [randomAddressDataArrayElement(), getStringArgBeforeFunc], [randomAliasDataArrayElement(), getString], [randomAliasDataArrayElement(), getStringArgBeforeFunc], [randomAddressDataArrayElement(), getStringValue], [randomAddressDataArrayElement(), getStringValueArgBeforeFunc], [randomAliasDataArrayElement(), getStringValue], [randomAliasDataArrayElement(), getStringValueArgBeforeFunc], [thisVariable, getString], [thisVariable, getStringArgBeforeFunc], [thisVariable, getStringValue], [thisVariable, getStringValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, stringData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-17. Compile own data getString functions for address, alias, and 'this'", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("String", version)
        for (const ownData of [ownDataGetString, ownDataGetStringArgBeforeFunc, ownDataGetStringValueArgBeforeFunc, ownDataGetStringValue]) {
          const script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-18. Test negative scenarios for getString functions", () => {
      const invalidFunction = `getStringValue(${randomInt()})`
      const invalidArgBeforeFunction = `${randomInt()}.getStringValue()`
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [addressOrAlias, stringData] of [[randomAddressDataArrayElement(), invalidGetString], [randomAliasDataArrayElement(), invalidGetStringValue], [randomInt(), getString], [randomInt(), getStringValue], [randomInt(), invalidFunction], [randomInt(), invalidArgBeforeFunction]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, stringData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-19. Ensure no overload of own data Integer accountDataStorage for old versions", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [addressOrAlias, stringData] of [[randomAddressDataArrayElement(), ownDataGetStringValue], [randomAliasDataArrayElement(), ownDataGetStringValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, stringData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
