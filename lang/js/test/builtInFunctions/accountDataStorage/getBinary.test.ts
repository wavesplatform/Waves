import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("GetBinary", () => {
  // getBinary
  const getBinary = `getBinary(callerTestData, ${randomStringArrayElement()})`
  const getBinaryArgBeforeFunc = `callerTestData.getBinary(${randomStringArrayElement()})`
  const ownDataGetBinary = `getBinary(${randomStringArrayElement()})`
  const ownDataGetBinaryArgBeforeFunc = `${randomStringArrayElement()}.getBinary()`

  // getBinaryValue
  const getBinaryValue = `getBinaryValue(callerTestData, ${randomStringArrayElement()})`
  const getBinaryValueArgBeforeFunc = `callerTestData.getBinaryValue(${randomStringArrayElement()})`
  const ownDataGetBinaryValue = `getBinaryValue(${randomStringArrayElement()})`
  const ownDataGetBinaryValueArgBeforeFunc = `${randomStringArrayElement()}.getBinaryValue()`

  const invalidGetBinary = `getBinary(callerTestData)`
  const invalidGetBinaryValue = `getBinaryValue(callerTestData)`

    test("RIDE-4. Compile getBinary functions for address, alias, and 'this'", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [addressOrAlias, binary] of [[randomAddressDataArrayElement(), getBinary], [randomAddressDataArrayElement(), getBinaryArgBeforeFunc], [randomAliasDataArrayElement(), getBinary], [randomAliasDataArrayElement(), getBinaryArgBeforeFunc], [randomAddressDataArrayElement(), getBinaryValue], [randomAddressDataArrayElement(), getBinaryValueArgBeforeFunc], [randomAliasDataArrayElement(), getBinaryValue], [randomAliasDataArrayElement(), getBinaryValueArgBeforeFunc], [thisVariable, getBinary], [thisVariable, getBinaryArgBeforeFunc], [thisVariable, getBinaryValue], [thisVariable, getBinaryValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-5. Compile own data getBinary functions for address, alias, and 'this'", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const ownData of [ownDataGetBinary, ownDataGetBinaryArgBeforeFunc, ownDataGetBinaryValueArgBeforeFunc, ownDataGetBinaryValue]) {
          const script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-6. Test negative scenarios for getBinary functions", () => {
      const invalidFunction = `getBinaryValue(${randomInt()})`
      const invalidArgBeforeFunction = `${randomInt()}.getBinaryValue()`
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [addressOrAlias, binary] of [[randomAddressDataArrayElement(), invalidGetBinary], [randomAliasDataArrayElement(), invalidGetBinaryValue], [randomInt(), getBinary], [randomInt(), getBinaryValue], [randomInt(), invalidFunction], [randomInt(), invalidArgBeforeFunction]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-7. Ensure no overload of own data Binary accountDataStorage for old versions", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [addressOrAlias, binary] of [[randomAddressDataArrayElement(), ownDataGetBinaryValue], [randomAliasDataArrayElement(), ownDataGetBinaryValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(addressOrAlias, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
