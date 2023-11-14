package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersionsWithoutV3, binaryEntryForTests, booleanEntryForTests, integerEntryForTests, rideV3Result, stringEntryForTests}
import utest.{Tests, test}

object GetString extends JsTestBase {
  // getStringKey
  private val getStringKey = s"getString(callerTestData, \"key\")"
  private val getStringKeyArgBeforeFunc = s"callerTestData.getString(\"key\")"
  // getStringIndex
  private val getStringIndex = s"getString(callerTestData, $randomInt)"
  private val getStringIndexArgBeforeFunc = s"callerTestData.getString($randomInt)"
  // getStringValueKey
  private val getStringValueKey = s"getStringValue(callerTestData, \"key\")"
  private val getStringValueKeyArgBeforeFunc = s"callerTestData.getStringValue(\"key\")"
  // getStringValueIndex
  private val getStringValueIndex = s"getStringValue(callerTestData, $randomInt)"
  private val getStringValueIndexArgBeforeFunc = s"callerTestData.getStringValue($randomInt)"

  // invalid getString
  private val invalidGetStringKey = s"getString()"
  private val invalidGetStringArgBeforeFunc = s"callerTestData.getString()"
  // invalid getStringValue
  private val invalidGetStringValue = s"getStringValue()"
  private val invalidGetStringValueArgBeforeFunc = s"callerTestData.getStringValue()"

  val tests: Tests = Tests {
    test("RIDE-110. getString functions for dataTransaction should compile for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, binary) <- Seq(
            (binaryEntryForTests, getStringKey),
            (integerEntryForTests, getStringKey),
            (stringEntryForTests, getStringKey),
            (booleanEntryForTests, getStringKey),
            (binaryEntryForTests, getStringKeyArgBeforeFunc),
            (integerEntryForTests, getStringKeyArgBeforeFunc),
            (stringEntryForTests, getStringKeyArgBeforeFunc),
            (booleanEntryForTests, getStringKeyArgBeforeFunc),
            (binaryEntryForTests, getStringIndex),
            (integerEntryForTests, getStringIndex),
            (stringEntryForTests, getStringIndex),
            (booleanEntryForTests, getStringIndex),
            (binaryEntryForTests, getStringIndexArgBeforeFunc),
            (integerEntryForTests, getStringIndexArgBeforeFunc),
            (stringEntryForTests, getStringIndexArgBeforeFunc),
            (booleanEntryForTests, getStringIndexArgBeforeFunc),
            (binaryEntryForTests, getStringValueKey),
            (integerEntryForTests, getStringValueKey),
            (stringEntryForTests, getStringValueKey),
            (booleanEntryForTests, getStringValueKey),
            (binaryEntryForTests, getStringValueKeyArgBeforeFunc),
            (integerEntryForTests, getStringValueKeyArgBeforeFunc),
            (stringEntryForTests, getStringValueKeyArgBeforeFunc),
            (booleanEntryForTests, getStringValueKeyArgBeforeFunc),
            (binaryEntryForTests, getStringValueIndex),
            (integerEntryForTests, getStringValueIndex),
            (stringEntryForTests, getStringValueIndex),
            (booleanEntryForTests, getStringValueIndex),
            (binaryEntryForTests, getStringValueIndexArgBeforeFunc),
            (integerEntryForTests, getStringValueIndexArgBeforeFunc),
            (stringEntryForTests, getStringValueIndexArgBeforeFunc),
            (booleanEntryForTests, getStringValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-111. getString function for dataTransaction should compile for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), getStringKey),
          (dataEntryForTests(randomStringArrayElement), getStringKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getStringIndex),
          (dataEntryForTests(randomStringArrayElement), getStringIndexArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getStringValueKey),
          (dataEntryForTests(randomStringArrayElement), getStringValueKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getStringValueIndex),
          (dataEntryForTests(randomStringArrayElement), getStringValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
        assertCompileSuccessDApp(script, V3)
      }
    }

    test("RIDE-112. getString function should throw an error for invalid data type for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, binary) <- Seq(
            (randomInt.toString, getStringKey),
            (randomBoolean.toString, getStringKeyArgBeforeFunc),
            (randomInt.toString, getStringIndex),
            (randomBoolean.toString, getStringIndexArgBeforeFunc),
            (randomInt.toString, getStringValueKey),
            (randomBoolean.toString, getStringValueKeyArgBeforeFunc),
            (randomInt.toString, getStringValueIndex),
            (randomBoolean.toString, getStringValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-113. getString function should throw an error for invalid data type for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      for (
        (data, binary) <- Seq(
          (randomInt.toString, getStringKey),
          (randomBoolean.toString, getStringKeyArgBeforeFunc),
          (randomInt.toString, getStringIndex),
          (randomBoolean.toString, getStringIndexArgBeforeFunc),
          (randomInt.toString, getStringValueKey),
          (randomBoolean.toString, getStringValueKeyArgBeforeFunc),
          (randomInt.toString, getStringValueIndex),
          (randomBoolean.toString, getStringValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("RIDE-114. Invalid getString functions should not compile for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, binary) <- Seq(
            (integerEntryForTests, invalidGetStringKey),
            (binaryEntryForTests, invalidGetStringArgBeforeFunc),
            (integerEntryForTests, invalidGetStringValue),
            (binaryEntryForTests, invalidGetStringValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-115. Invalid getString functions should not compile for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), invalidGetStringKey),
          (dataEntryForTests(randomStringArrayElement), invalidGetStringArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), invalidGetStringValue),
          (dataEntryForTests(randomStringArrayElement), invalidGetStringValueArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultStringEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
