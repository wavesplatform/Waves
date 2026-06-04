import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultIntegerEntry, actualVersions, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("GetInteger", () => {
  // getInteger
  const getInteger = `getInteger(callerTestData, ${randomStringArrayElement()})`
  const getIntegerArgBeforeFunc = `callerTestData.getInteger(${randomStringArrayElement()})`
  const ownDataGetInt = `getInteger(${randomStringArrayElement()})`
  const ownDataGetIntArgBeforeFunc = `${randomStringArrayElement()}.getInteger()`

  // getIntegerValue
  const getIntegerValue = `getIntegerValue(callerTestData, ${randomStringArrayElement()})`
  const getIntegerValueArgBeforeFunc = `callerTestData.getIntegerValue(${randomStringArrayElement()})`
  const ownDataGetIntValue = `getIntegerValue(${randomStringArrayElement()})`
  const ownDataGetIntValueArgBeforeFunc = `${randomStringArrayElement()}.getIntegerValue()`

  const invalidGetInt = `getInteger(callerTestData)`
  const invalidGetIntValue = `getIntegerValue(callerTestData)`

    test("RIDE-12. Compile getInteger functions for address, alias, and 'this'", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [addressOrAlias, intData] of [[randomAddressDataArrayElement(), getInteger], [randomAddressDataArrayElement(), getIntegerArgBeforeFunc], [randomAliasDataArrayElement(), getInteger], [randomAliasDataArrayElement(), getIntegerArgBeforeFunc], [randomAddressDataArrayElement(), getIntegerValue], [randomAddressDataArrayElement(), getIntegerValueArgBeforeFunc], [randomAliasDataArrayElement(), getIntegerValue], [randomAliasDataArrayElement(), getIntegerValueArgBeforeFunc], [thisVariable, getInteger], [thisVariable, getIntegerArgBeforeFunc], [thisVariable, getIntegerValue], [thisVariable, getIntegerValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, intData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-13. Compile own data getInteger functions for address, alias, and 'this'", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("Int", version)
        for (const ownData of [ownDataGetInt, ownDataGetIntArgBeforeFunc, ownDataGetIntValueArgBeforeFunc, ownDataGetIntValue]) {
          const script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-14. Test negative scenarios for getInteger functions", () => {
      const invalidFunction = `getIntegerValue(${randomInt()})`
      const invalidArgBeforeFunction = `${randomInt()}.getIntegerValue()`
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [addressOrAlias, intData] of [[randomAddressDataArrayElement(), invalidGetInt], [randomAliasDataArrayElement(), invalidGetIntValue], [randomInt(), getInteger], [randomInt(), getIntegerValue], [randomInt(), invalidFunction], [randomInt(), invalidArgBeforeFunction]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, intData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-15. Ensure no overload of own data Integer accountDataStorage for old versions", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [addressOrAlias, intData] of [[randomAddressDataArrayElement(), ownDataGetIntValue], [randomAliasDataArrayElement(), ownDataGetIntValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, intData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
