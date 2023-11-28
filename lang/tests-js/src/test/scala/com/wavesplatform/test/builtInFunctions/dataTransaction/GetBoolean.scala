package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBooleanEntry, actualVersionsWithoutV3, binaryEntryForTests, booleanEntryForTests, integerEntryForTests, rideV3Result, stringEntryForTests}
import utest.{Tests, test}

object GetBoolean extends JsTestBase {
  // getBooleanKey
  private val getBooleanKey = s"getBoolean(callerTestData, \"key\")"
  private val getBooleanKeyArgBeforeFunc = s"callerTestData.getBoolean(\"key\")"
  // getBooleanIndex
  private val getBooleanIndex = s"getBoolean(callerTestData, $randomInt)"
  private val getBooleanIndexArgBeforeFunc = s"callerTestData.getBoolean($randomInt)"
  // getBooleanValueKey
  private val getBooleanValueKey = s"getBooleanValue(callerTestData, \"key\")"
  private val getBooleanValueKeyArgBeforeFunc = s"callerTestData.getBooleanValue(\"key\")"
  // getBooleanValueIndex
  private val getBooleanValueIndex = s"getBooleanValue(callerTestData, $randomInt)"
  private val getBooleanValueIndexArgBeforeFunc = s"callerTestData.getBooleanValue($randomInt)"

  // invalid getBoolean
  private val invalidGetBooleanKey = s"getBoolean()"
  private val invalidGetBooleanArgBeforeFunc = s"callerTestData.getBoolean()"
  // invalid getBooleanValue
  private val invalidGetBooleanValue = s"getBooleanValue()"
  private val invalidGetBooleanValueArgBeforeFunc = s"callerTestData.getBooleanValue()"

  val tests: Tests = Tests {
    test("RIDE-98. getBoolean functions for dataTransaction should compile for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, binary) <- Seq(
            (binaryEntryForTests, getBooleanKey),
            (integerEntryForTests, getBooleanKey),
            (stringEntryForTests, getBooleanKey),
            (booleanEntryForTests, getBooleanKey),
            (binaryEntryForTests, getBooleanKeyArgBeforeFunc),
            (integerEntryForTests, getBooleanKeyArgBeforeFunc),
            (stringEntryForTests, getBooleanKeyArgBeforeFunc),
            (booleanEntryForTests, getBooleanKeyArgBeforeFunc),
            (binaryEntryForTests, getBooleanIndex),
            (integerEntryForTests, getBooleanIndex),
            (stringEntryForTests, getBooleanIndex),
            (booleanEntryForTests, getBooleanIndex),
            (binaryEntryForTests, getBooleanIndexArgBeforeFunc),
            (integerEntryForTests, getBooleanIndexArgBeforeFunc),
            (stringEntryForTests, getBooleanIndexArgBeforeFunc),
            (booleanEntryForTests, getBooleanIndexArgBeforeFunc),
            (binaryEntryForTests, getBooleanValueKey),
            (integerEntryForTests, getBooleanValueKey),
            (stringEntryForTests, getBooleanValueKey),
            (booleanEntryForTests, getBooleanValueKey),
            (binaryEntryForTests, getBooleanValueKeyArgBeforeFunc),
            (integerEntryForTests, getBooleanValueKeyArgBeforeFunc),
            (stringEntryForTests, getBooleanValueKeyArgBeforeFunc),
            (booleanEntryForTests, getBooleanValueKeyArgBeforeFunc),
            (binaryEntryForTests, getBooleanValueIndex),
            (integerEntryForTests, getBooleanValueIndex),
            (stringEntryForTests, getBooleanValueIndex),
            (booleanEntryForTests, getBooleanValueIndex),
            (binaryEntryForTests, getBooleanValueIndexArgBeforeFunc),
            (integerEntryForTests, getBooleanValueIndexArgBeforeFunc),
            (stringEntryForTests, getBooleanValueIndexArgBeforeFunc),
            (booleanEntryForTests, getBooleanValueIndexArgBeforeFunc),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-99. getBoolean function for dataTransaction should compile for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), getBooleanKey),
          (dataEntryForTests(randomStringArrayElement), getBooleanKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getBooleanIndex),
          (dataEntryForTests(randomStringArrayElement), getBooleanIndexArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getBooleanValueKey),
          (dataEntryForTests(randomStringArrayElement), getBooleanValueKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getBooleanValueIndex),
          (dataEntryForTests(randomStringArrayElement), getBooleanValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
        assertCompileSuccessDApp(script, V3)
      }
    }

    test("RIDE-100. getBoolean function should throw an error for invalid data type for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, binary) <- Seq(
            (randomInt.toString, getBooleanKey),
            (randomBoolean.toString, getBooleanKeyArgBeforeFunc),
            (randomInt.toString, getBooleanIndex),
            (randomBoolean.toString, getBooleanIndexArgBeforeFunc),
            (randomInt.toString, getBooleanValueKey),
            (randomBoolean.toString, getBooleanValueKeyArgBeforeFunc),
            (randomInt.toString, getBooleanValueIndex),
            (randomBoolean.toString, getBooleanValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-101. getBoolean function should throw an error for invalid data type for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      for (
        (data, binary) <- Seq(
          (randomInt.toString, getBooleanKey),
          (randomBoolean.toString, getBooleanKeyArgBeforeFunc),
          (randomInt.toString, getBooleanIndex),
          (randomBoolean.toString, getBooleanIndexArgBeforeFunc),
          (randomInt.toString, getBooleanValueKey),
          (randomBoolean.toString, getBooleanValueKeyArgBeforeFunc),
          (randomInt.toString, getBooleanValueIndex),
          (randomBoolean.toString, getBooleanValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("RIDE-102. Invalid getBoolean functions should not compile for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, binary) <- Seq(
            (integerEntryForTests, invalidGetBooleanKey),
            (binaryEntryForTests, invalidGetBooleanArgBeforeFunc),
            (integerEntryForTests, invalidGetBooleanValue),
            (binaryEntryForTests, invalidGetBooleanValueArgBeforeFunc),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-103. Invalid getBoolean functions should not compile for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), invalidGetBooleanKey),
          (dataEntryForTests(randomStringArrayElement), invalidGetBooleanArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), invalidGetBooleanValue),
          (dataEntryForTests(randomStringArrayElement), invalidGetBooleanValueArgBeforeFunc),
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBooleanEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
