import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultIntegerEntry, actualVersionsWithoutV3, binaryEntryForTests, booleanEntryForTests, integerEntryForTests, rideV3Result, stringEntryForTests } from "../../helpers/testData";

describe("GetInteger", () => {
  // getIntegerKey
  const getIntegerKey = `getInteger(callerTestData, \"key\")`
  const getIntegerKeyArgBeforeFunc = `callerTestData.getInteger(\"key\")`
  // getIntegerIndex
  const getIntegerIndex = `getInteger(callerTestData, ${randomInt()})`
  const getIntegerIndexArgBeforeFunc = `callerTestData.getInteger(${randomInt()})`
  // getIntegerValueKey
  const getIntegerValueKey = `getIntegerValue(callerTestData, \"key\")`
  const getIntegerValueKeyArgBeforeFunc = `callerTestData.getIntegerValue(\"key\")`
  // getIntegerValueIndex
  const getIntegerValueIndex = `getIntegerValue(callerTestData, ${randomInt()})`
  const getIntegerValueIndexArgBeforeFunc = `callerTestData.getIntegerValue(${randomInt()})`

  // invalid getInteger
  const invalidGetIntegerKey = `getInteger()`
  const invalidGetIntegerArgBeforeFunc = `callerTestData.getInteger()`
  // invalid getIntegerValue
  const invalidGetIntegerValue = `getIntegerValue()`
  const invalidGetIntegerValueArgBeforeFunc = `callerTestData.getIntegerValue()`

    test("RIDE-104. getInteger functions for dataTransaction should compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, binary] of [[binaryEntryForTests, getIntegerKey], [integerEntryForTests, getIntegerKey], [stringEntryForTests, getIntegerKey], [booleanEntryForTests, getIntegerKey], [binaryEntryForTests, getIntegerKeyArgBeforeFunc], [integerEntryForTests, getIntegerKeyArgBeforeFunc], [stringEntryForTests, getIntegerKeyArgBeforeFunc], [booleanEntryForTests, getIntegerKeyArgBeforeFunc], [binaryEntryForTests, getIntegerIndex], [integerEntryForTests, getIntegerIndex], [stringEntryForTests, getIntegerIndex], [booleanEntryForTests, getIntegerIndex], [binaryEntryForTests, getIntegerIndexArgBeforeFunc], [integerEntryForTests, getIntegerIndexArgBeforeFunc], [stringEntryForTests, getIntegerIndexArgBeforeFunc], [booleanEntryForTests, getIntegerIndexArgBeforeFunc], [binaryEntryForTests, getIntegerValueKey], [integerEntryForTests, getIntegerValueKey], [stringEntryForTests, getIntegerValueKey], [booleanEntryForTests, getIntegerValueKey], [binaryEntryForTests, getIntegerValueKeyArgBeforeFunc], [integerEntryForTests, getIntegerValueKeyArgBeforeFunc], [stringEntryForTests, getIntegerValueKeyArgBeforeFunc], [booleanEntryForTests, getIntegerValueKeyArgBeforeFunc], [binaryEntryForTests, getIntegerValueIndex], [integerEntryForTests, getIntegerValueIndex], [stringEntryForTests, getIntegerValueIndex], [booleanEntryForTests, getIntegerValueIndex], [binaryEntryForTests, getIntegerValueIndexArgBeforeFunc], [integerEntryForTests, getIntegerValueIndexArgBeforeFunc], [stringEntryForTests, getIntegerValueIndexArgBeforeFunc], [booleanEntryForTests, getIntegerValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-105. getInteger function for dataTransaction should compile for V3", () => {
      const precondition = new ContractGenerator("Int", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), getIntegerKey], [dataEntryForTests(randomStringArrayElement()), getIntegerKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getIntegerIndex], [dataEntryForTests(randomStringArrayElement()), getIntegerIndexArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getIntegerValueKey], [dataEntryForTests(randomStringArrayElement()), getIntegerValueKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getIntegerValueIndex], [dataEntryForTests(randomStringArrayElement()), getIntegerValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
        assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-106. getInteger function should throw an error for invalid data type for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, binary] of [[randomInt(), getIntegerKey], [randomBoolean(), getIntegerKeyArgBeforeFunc], [randomInt(), getIntegerIndex], [randomBoolean(), getIntegerIndexArgBeforeFunc], [randomInt(), getIntegerValueKey], [randomBoolean(), getIntegerValueKeyArgBeforeFunc], [randomInt(), getIntegerValueIndex], [randomBoolean(), getIntegerValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-107. getInteger function should throw an error for invalid data type for V3", () => {
      const precondition = new ContractGenerator("Int", V3)
      for (const [data, binary] of [[randomInt(), getIntegerKey], [randomBoolean(), getIntegerKeyArgBeforeFunc], [randomInt(), getIntegerIndex], [randomBoolean(), getIntegerIndexArgBeforeFunc], [randomInt(), getIntegerValueKey], [randomBoolean(), getIntegerValueKeyArgBeforeFunc], [randomInt(), getIntegerValueIndex], [randomBoolean(), getIntegerValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });

    test("RIDE-108. Invalid getInteger functions should not compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, binary] of [[integerEntryForTests, invalidGetIntegerKey], [binaryEntryForTests, invalidGetIntegerArgBeforeFunc], [integerEntryForTests, invalidGetIntegerValue], [binaryEntryForTests, invalidGetIntegerValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-109. Invalid getInteger functions should not compile for V3", () => {
      const precondition = new ContractGenerator("Int", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), invalidGetIntegerKey], [dataEntryForTests(randomStringArrayElement()), invalidGetIntegerArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), invalidGetIntegerValue], [dataEntryForTests(randomStringArrayElement()), invalidGetIntegerValueArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
  });
});
