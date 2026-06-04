import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersionsWithoutV3, binaryEntryForTests, booleanEntryForTests, integerEntryForTests, rideV3Result, stringEntryForTests } from "../../helpers/testData";

describe("GetString", () => {
  // getStringKey
  const getStringKey = `getString(callerTestData, \"key\")`
  const getStringKeyArgBeforeFunc = `callerTestData.getString(\"key\")`
  // getStringIndex
  const getStringIndex = `getString(callerTestData, ${randomInt()})`
  const getStringIndexArgBeforeFunc = `callerTestData.getString(${randomInt()})`
  // getStringValueKey
  const getStringValueKey = `getStringValue(callerTestData, \"key\")`
  const getStringValueKeyArgBeforeFunc = `callerTestData.getStringValue(\"key\")`
  // getStringValueIndex
  const getStringValueIndex = `getStringValue(callerTestData, ${randomInt()})`
  const getStringValueIndexArgBeforeFunc = `callerTestData.getStringValue(${randomInt()})`

  // invalid getString
  const invalidGetStringKey = `getString()`
  const invalidGetStringArgBeforeFunc = `callerTestData.getString()`
  // invalid getStringValue
  const invalidGetStringValue = `getStringValue()`
  const invalidGetStringValueArgBeforeFunc = `callerTestData.getStringValue()`

    test("RIDE-110. getString functions for dataTransaction should compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, binary] of [[binaryEntryForTests, getStringKey], [integerEntryForTests, getStringKey], [stringEntryForTests, getStringKey], [booleanEntryForTests, getStringKey], [binaryEntryForTests, getStringKeyArgBeforeFunc], [integerEntryForTests, getStringKeyArgBeforeFunc], [stringEntryForTests, getStringKeyArgBeforeFunc], [booleanEntryForTests, getStringKeyArgBeforeFunc], [binaryEntryForTests, getStringIndex], [integerEntryForTests, getStringIndex], [stringEntryForTests, getStringIndex], [booleanEntryForTests, getStringIndex], [binaryEntryForTests, getStringIndexArgBeforeFunc], [integerEntryForTests, getStringIndexArgBeforeFunc], [stringEntryForTests, getStringIndexArgBeforeFunc], [booleanEntryForTests, getStringIndexArgBeforeFunc], [binaryEntryForTests, getStringValueKey], [integerEntryForTests, getStringValueKey], [stringEntryForTests, getStringValueKey], [booleanEntryForTests, getStringValueKey], [binaryEntryForTests, getStringValueKeyArgBeforeFunc], [integerEntryForTests, getStringValueKeyArgBeforeFunc], [stringEntryForTests, getStringValueKeyArgBeforeFunc], [booleanEntryForTests, getStringValueKeyArgBeforeFunc], [binaryEntryForTests, getStringValueIndex], [integerEntryForTests, getStringValueIndex], [stringEntryForTests, getStringValueIndex], [booleanEntryForTests, getStringValueIndex], [binaryEntryForTests, getStringValueIndexArgBeforeFunc], [integerEntryForTests, getStringValueIndexArgBeforeFunc], [stringEntryForTests, getStringValueIndexArgBeforeFunc], [booleanEntryForTests, getStringValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-111. getString function for dataTransaction should compile for V3", () => {
      const precondition = new ContractGenerator("String", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), getStringKey], [dataEntryForTests(randomStringArrayElement()), getStringKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getStringIndex], [dataEntryForTests(randomStringArrayElement()), getStringIndexArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getStringValueKey], [dataEntryForTests(randomStringArrayElement()), getStringValueKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getStringValueIndex], [dataEntryForTests(randomStringArrayElement()), getStringValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
        assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-112. getString function should throw an error for invalid data type for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, binary] of [[randomInt(), getStringKey], [randomBoolean(), getStringKeyArgBeforeFunc], [randomInt(), getStringIndex], [randomBoolean(), getStringIndexArgBeforeFunc], [randomInt(), getStringValueKey], [randomBoolean(), getStringValueKeyArgBeforeFunc], [randomInt(), getStringValueIndex], [randomBoolean(), getStringValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-113. getString function should throw an error for invalid data type for V3", () => {
      const precondition = new ContractGenerator("String", V3)
      for (const [data, binary] of [[randomInt(), getStringKey], [randomBoolean(), getStringKeyArgBeforeFunc], [randomInt(), getStringIndex], [randomBoolean(), getStringIndexArgBeforeFunc], [randomInt(), getStringValueKey], [randomBoolean(), getStringValueKeyArgBeforeFunc], [randomInt(), getStringValueIndex], [randomBoolean(), getStringValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });

    test("RIDE-114. Invalid getString functions should not compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, binary] of [[integerEntryForTests, invalidGetStringKey], [binaryEntryForTests, invalidGetStringArgBeforeFunc], [integerEntryForTests, invalidGetStringValue], [binaryEntryForTests, invalidGetStringValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-115. Invalid getString functions should not compile for V3", () => {
      const precondition = new ContractGenerator("String", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), invalidGetStringKey], [dataEntryForTests(randomStringArrayElement()), invalidGetStringArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), invalidGetStringValue], [dataEntryForTests(randomStringArrayElement()), invalidGetStringValueArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });
});
