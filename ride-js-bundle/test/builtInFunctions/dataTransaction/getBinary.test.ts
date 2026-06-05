import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, binaryEntryForTests, booleanEntryForTests, integerEntryForTests, rideV3Result, stringEntryForTests } from "../../helpers/testData";

describe("GetBinary", () => {
  // getBinaryKey
  const getBinaryKey = `getBinary(callerTestData, \"key\")`
  const getBinaryKeyArgBeforeFunc = `callerTestData.getBinary(\"key\")`
  // getBinaryIndex
  const getBinaryIndex = `getBinary(callerTestData, ${randomInt()})`
  const getBinaryIndexArgBeforeFunc = `callerTestData.getBinary(${randomInt()})`
  // getBinaryValueKey
  const getBinaryValueKey = `getBinaryValue(callerTestData, \"key\")`
  const getBinaryValueKeyArgBeforeFunc = `callerTestData.getBinaryValue(\"key\")`
  // getBinaryValueIndex
  const getBinaryValueIndex = `getBinaryValue(callerTestData, ${randomInt()})`
  const getBinaryValueIndexArgBeforeFunc = `callerTestData.getBinaryValue(${randomInt()})`

  // invalid getBinary
  const invalidGetBinaryKey = `getBinary()`
  const invalidGetBinaryArgBeforeFunc = `callerTestData.getBinary()`
  // invalid getBinaryValue
  const invalidGetBinaryValue = `getBinaryValue()`
  const invalidGetBinaryValueArgBeforeFunc = `callerTestData.getBinaryValue()`

    test("RIDE-92. getBinary functions for dataTransaction should compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, binary] of [[binaryEntryForTests, getBinaryKey], [integerEntryForTests, getBinaryKey], [stringEntryForTests, getBinaryKey], [booleanEntryForTests, getBinaryKey], [binaryEntryForTests, getBinaryKeyArgBeforeFunc], [integerEntryForTests, getBinaryKeyArgBeforeFunc], [stringEntryForTests, getBinaryKeyArgBeforeFunc], [booleanEntryForTests, getBinaryKeyArgBeforeFunc], [binaryEntryForTests, getBinaryIndex], [integerEntryForTests, getBinaryIndex], [stringEntryForTests, getBinaryIndex], [booleanEntryForTests, getBinaryIndex], [binaryEntryForTests, getBinaryIndexArgBeforeFunc], [integerEntryForTests, getBinaryIndexArgBeforeFunc], [stringEntryForTests, getBinaryIndexArgBeforeFunc], [booleanEntryForTests, getBinaryIndexArgBeforeFunc], [binaryEntryForTests, getBinaryValueKey], [integerEntryForTests, getBinaryValueKey], [stringEntryForTests, getBinaryValueKey], [booleanEntryForTests, getBinaryValueKey], [binaryEntryForTests, getBinaryValueKeyArgBeforeFunc], [integerEntryForTests, getBinaryValueKeyArgBeforeFunc], [stringEntryForTests, getBinaryValueKeyArgBeforeFunc], [booleanEntryForTests, getBinaryValueKeyArgBeforeFunc], [binaryEntryForTests, getBinaryValueIndex], [integerEntryForTests, getBinaryValueIndex], [stringEntryForTests, getBinaryValueIndex], [booleanEntryForTests, getBinaryValueIndex], [binaryEntryForTests, getBinaryValueIndexArgBeforeFunc], [integerEntryForTests, getBinaryValueIndexArgBeforeFunc], [stringEntryForTests, getBinaryValueIndexArgBeforeFunc], [booleanEntryForTests, getBinaryValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-93. getBinary function for dataTransaction should compile for V3", () => {
      const precondition = new ContractGenerator("ByteVector", V3)
      for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), getBinaryKey], [dataEntryForTests(randomStringArrayElement()), getBinaryKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getBinaryIndex], [dataEntryForTests(randomStringArrayElement()), getBinaryIndexArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getBinaryValueKey], [dataEntryForTests(randomStringArrayElement()), getBinaryValueKeyArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), getBinaryValueIndex], [dataEntryForTests(randomStringArrayElement()), getBinaryValueIndexArgBeforeFunc]]) {
        const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
        assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-94. getBinary function should throw an error for invalid data type for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, binary] of [[randomInt(), getBinaryKey], [randomBoolean(), getBinaryKeyArgBeforeFunc], [randomInt(), getBinaryIndex], [randomBoolean(), getBinaryIndexArgBeforeFunc], [randomInt(), getBinaryValueKey], [randomBoolean(), getBinaryValueKeyArgBeforeFunc], [randomInt(), getBinaryValueIndex], [randomBoolean(), getBinaryValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-95. getBinary function should throw an error for invalid data type for V3", () => {
        const precondition = new ContractGenerator("ByteVector", V3)
        for (const [data, binary] of [[randomInt(), getBinaryKey], [randomBoolean(), getBinaryKeyArgBeforeFunc], [randomInt(), getBinaryIndex], [randomBoolean(), getBinaryIndexArgBeforeFunc], [randomInt(), getBinaryValueKey], [randomBoolean(), getBinaryValueKeyArgBeforeFunc], [randomInt(), getBinaryValueIndex], [randomBoolean(), getBinaryValueIndexArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
  });

    test("RIDE-96. Invalid getBinary functions should not compile for versions V4 and above", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, binary] of [[integerEntryForTests, invalidGetBinaryKey], [binaryEntryForTests, invalidGetBinaryArgBeforeFunc], [integerEntryForTests, invalidGetBinaryValue], [binaryEntryForTests, invalidGetBinaryValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });

    test("RIDE-97. Invalid getBinary functions should not compile for V3", () => {
        const precondition = new ContractGenerator("ByteVector", V3)
        for (const [data, binary] of [[dataEntryForTests(randomStringArrayElement()), invalidGetBinaryKey], [dataEntryForTests(randomStringArrayElement()), invalidGetBinaryArgBeforeFunc], [dataEntryForTests(randomStringArrayElement()), invalidGetBinaryValue], [dataEntryForTests(randomStringArrayElement()), invalidGetBinaryValueArgBeforeFunc]]) {
          const script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
  });
});
