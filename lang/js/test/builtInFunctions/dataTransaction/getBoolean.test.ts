import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBooleanEntry, actualVersionsWithoutV3, binaryEntryForTests, booleanEntryForTests, integerEntryForTests, rideV3Result, stringEntryForTests } from "../../helpers/testData";

describe("GetBoolean", () => {
  // getBooleanKey
  const getBooleanKey = `getBoolean(callerTestData, \"key\")`
  const getBooleanKeyArgBeforeFunc = `callerTestData.getBoolean(\"key\")`
  // getBooleanIndex
  const getBooleanIndex = `getBoolean(callerTestData, ${randomInt()})`
  const getBooleanIndexArgBeforeFunc = `callerTestData.getBoolean(${randomInt()})`
  // getBooleanValueKey
  const getBooleanValueKey = `getBooleanValue(callerTestData, \"key\")`
  const getBooleanValueKeyArgBeforeFunc = `callerTestData.getBooleanValue(\"key\")`
  // getBooleanValueIndex
  const getBooleanValueIndex = `getBooleanValue(callerTestData, ${randomInt()})`
  const getBooleanValueIndexArgBeforeFunc = `callerTestData.getBooleanValue(${randomInt()})`

  // invalid getBoolean
  const invalidGetBooleanKey = `getBoolean()`
  const invalidGetBooleanArgBeforeFunc = `callerTestData.getBoolean()`
  // invalid getBooleanValue
  const invalidGetBooleanValue = `getBooleanValue()`
  const invalidGetBooleanValueArgBeforeFunc = `callerTestData.getBooleanValue()`

    test("RIDE-98. getBoolean functions for dataTransaction should compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, binary] of [[binaryEntryForTests, getBooleanKey], [integerEntryForTests, getBooleanKey], [stringEntryForTests, getBooleanKey], [booleanEntryForTests, getBooleanKey], [binaryEntryForTests, getBooleanKeyArgBeforeFunc], [integerEntryForTests, getBooleanKeyArgBeforeFunc], [stringEntryForTests, getBooleanKeyArgBeforeFunc], [booleanEntryForTests, getBooleanKeyArgBeforeFunc], [binaryEntryForTests, getBooleanIndex], [integerEntryForTests, getBooleanIndex], [stringEntryForTests, getBooleanIndex], [booleanEntryForTests, getBooleanIndex], [binaryEntryForTests, getBooleanIndexArgBeforeFunc], [integerEntryForTests, getBooleanIndexArgBeforeFunc], [stringEntryForTests, getBooleanIndexArgBeforeFunc], [booleanEntryForTests, getBooleanIndexArgBeforeFunc], [binaryEntryForTests, getBooleanValueKey], [integerEntryForTests, getBooleanValueKey], [stringEntryForTests, getBooleanValueKey], [booleanEntryForTests, getBooleanValueKey], [binaryEntryForTests, getBooleanValueKeyArgBeforeFunc], [integerEntryForTests, getBooleanValueKeyArgBeforeFunc], [stringEntryForTests, getBooleanValueKeyArgBeforeFunc], [booleanEntryForTests, getBooleanValueKeyArgBeforeFunc], [binaryEntryForTests, getBooleanValueIndex], [integerEntryForTests, getBooleanValueIndex], [stringEntryForTests, getBooleanValueIndex], [booleanEntryForTests, getBooleanValueIndex], [binaryEntryForTests, getBooleanValueIndexArgBeforeFunc], [integerEntryForTests, getBooleanValueIndexArgBeforeFunc], [stringEntryForTests, getBooleanValueIndexArgBeforeFunc], [booleanEntryForTests, getBooleanValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-99. getBoolean function for dataTransaction should compile for V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), getBooleanKey], [dataEntryForTests(randomStringArrayElement()), getBooleanKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getBooleanIndex], [dataEntryForTests(randomStringArrayElement()), getBooleanIndexArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getBooleanValueKey], [dataEntryForTests(randomStringArrayElement()), getBooleanValueKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getBooleanValueIndex], [dataEntryForTests(randomStringArrayElement()), getBooleanValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
        assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-100. getBoolean function should throw an error for invalid data type for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, binary] of [[randomInt(), getBooleanKey], [randomBoolean(), getBooleanKeyArgBeforeFunc], [randomInt(), getBooleanIndex], [randomBoolean(), getBooleanIndexArgBeforeFunc], [randomInt(), getBooleanValueKey], [randomBoolean(), getBooleanValueKeyArgBeforeFunc], [randomInt(), getBooleanValueIndex], [randomBoolean(), getBooleanValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-101. getBoolean function should throw an error for invalid data type for V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      for (const [data, binary] of [[randomInt(), getBooleanKey], [randomBoolean(), getBooleanKeyArgBeforeFunc], [randomInt(), getBooleanIndex], [randomBoolean(), getBooleanIndexArgBeforeFunc], [randomInt(), getBooleanValueKey], [randomBoolean(), getBooleanValueKeyArgBeforeFunc], [randomInt(), getBooleanValueIndex], [randomBoolean(), getBooleanValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });

    test("RIDE-102. Invalid getBoolean functions should not compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, binary] of [[integerEntryForTests, invalidGetBooleanKey], [binaryEntryForTests, invalidGetBooleanArgBeforeFunc], [integerEntryForTests, invalidGetBooleanValue], [binaryEntryForTests, invalidGetBooleanValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-103. Invalid getBoolean functions should not compile for V3", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), invalidGetBooleanKey], [dataEntryForTests(randomStringArrayElement()), invalidGetBooleanArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), invalidGetBooleanValue], [dataEntryForTests(randomStringArrayElement()), invalidGetBooleanValueArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });
});
