package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_A_FUNCTION_OVERLOAD,
  GreaterV3ResultIntegerEntry,
  actualVersionsWithoutV3,
  binaryEntryForTests,
  booleanEntryForTests,
  integerEntryForTests,
  rideV3Result,
  stringEntryForTests
}
import utest.{Tests, test}

object GetInteger extends JsTestBase {
  // getIntegerKey
  private val getIntegerKey              = s"getInteger(callerTestData, \"key\")"
  private val getIntegerKeyArgBeforeFunc = s"callerTestData.getInteger(\"key\")"
  // getIntegerIndex
  private val getIntegerIndex              = s"getInteger(callerTestData, $randomInt)"
  private val getIntegerIndexArgBeforeFunc = s"callerTestData.getInteger($randomInt)"
  // getIntegerValueKey
  private val getIntegerValueKey              = s"getIntegerValue(callerTestData, \"key\")"
  private val getIntegerValueKeyArgBeforeFunc = s"callerTestData.getIntegerValue(\"key\")"
  // getIntegerValueIndex
  private val getIntegerValueIndex              = s"getIntegerValue(callerTestData, $randomInt)"
  private val getIntegerValueIndexArgBeforeFunc = s"callerTestData.getIntegerValue($randomInt)"

  // invalid getInteger
  private val invalidGetIntegerKey           = s"getInteger()"
  private val invalidGetIntegerArgBeforeFunc = s"callerTestData.getInteger()"
  // invalid getIntegerValue
  private val invalidGetIntegerValue              = s"getIntegerValue()"
  private val invalidGetIntegerValueArgBeforeFunc = s"callerTestData.getIntegerValue()"

  val tests: Tests = Tests {
    test("RIDE-104. getInteger functions for dataTransaction should compile for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, binary) <- Seq(
            (binaryEntryForTests, getIntegerKey),
            (integerEntryForTests, getIntegerKey),
            (stringEntryForTests, getIntegerKey),
            (booleanEntryForTests, getIntegerKey),
            (binaryEntryForTests, getIntegerKeyArgBeforeFunc),
            (integerEntryForTests, getIntegerKeyArgBeforeFunc),
            (stringEntryForTests, getIntegerKeyArgBeforeFunc),
            (booleanEntryForTests, getIntegerKeyArgBeforeFunc),
            (binaryEntryForTests, getIntegerIndex),
            (integerEntryForTests, getIntegerIndex),
            (stringEntryForTests, getIntegerIndex),
            (booleanEntryForTests, getIntegerIndex),
            (binaryEntryForTests, getIntegerIndexArgBeforeFunc),
            (integerEntryForTests, getIntegerIndexArgBeforeFunc),
            (stringEntryForTests, getIntegerIndexArgBeforeFunc),
            (booleanEntryForTests, getIntegerIndexArgBeforeFunc),
            (binaryEntryForTests, getIntegerValueKey),
            (integerEntryForTests, getIntegerValueKey),
            (stringEntryForTests, getIntegerValueKey),
            (booleanEntryForTests, getIntegerValueKey),
            (binaryEntryForTests, getIntegerValueKeyArgBeforeFunc),
            (integerEntryForTests, getIntegerValueKeyArgBeforeFunc),
            (stringEntryForTests, getIntegerValueKeyArgBeforeFunc),
            (booleanEntryForTests, getIntegerValueKeyArgBeforeFunc),
            (binaryEntryForTests, getIntegerValueIndex),
            (integerEntryForTests, getIntegerValueIndex),
            (stringEntryForTests, getIntegerValueIndex),
            (booleanEntryForTests, getIntegerValueIndex),
            (binaryEntryForTests, getIntegerValueIndexArgBeforeFunc),
            (integerEntryForTests, getIntegerValueIndexArgBeforeFunc),
            (stringEntryForTests, getIntegerValueIndexArgBeforeFunc),
            (booleanEntryForTests, getIntegerValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-105. getInteger function for dataTransaction should compile for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), getIntegerKey),
          (dataEntryForTests(randomStringArrayElement), getIntegerKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getIntegerIndex),
          (dataEntryForTests(randomStringArrayElement), getIntegerIndexArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getIntegerValueKey),
          (dataEntryForTests(randomStringArrayElement), getIntegerValueKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getIntegerValueIndex),
          (dataEntryForTests(randomStringArrayElement), getIntegerValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
        assertCompileSuccessDApp(script, V3)
      }
    }

    test("RIDE-106. getInteger function should throw an error for invalid data type for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, binary) <- Seq(
            (randomInt.toString, getIntegerKey),
            (randomBoolean.toString, getIntegerKeyArgBeforeFunc),
            (randomInt.toString, getIntegerIndex),
            (randomBoolean.toString, getIntegerIndexArgBeforeFunc),
            (randomInt.toString, getIntegerValueKey),
            (randomBoolean.toString, getIntegerValueKeyArgBeforeFunc),
            (randomInt.toString, getIntegerValueIndex),
            (randomBoolean.toString, getIntegerValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-107. getInteger function should throw an error for invalid data type for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      for (
        (data, binary) <- Seq(
          (randomInt.toString, getIntegerKey),
          (randomBoolean.toString, getIntegerKeyArgBeforeFunc),
          (randomInt.toString, getIntegerIndex),
          (randomBoolean.toString, getIntegerIndexArgBeforeFunc),
          (randomInt.toString, getIntegerValueKey),
          (randomBoolean.toString, getIntegerValueKeyArgBeforeFunc),
          (randomInt.toString, getIntegerValueIndex),
          (randomBoolean.toString, getIntegerValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("RIDE-108. Invalid getInteger functions should not compile for versions V4 and above") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, binary) <- Seq(
            (integerEntryForTests, invalidGetIntegerKey),
            (binaryEntryForTests, invalidGetIntegerArgBeforeFunc),
            (integerEntryForTests, invalidGetIntegerValue),
            (binaryEntryForTests, invalidGetIntegerValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-109. Invalid getInteger functions should not compile for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), invalidGetIntegerKey),
          (dataEntryForTests(randomStringArrayElement), invalidGetIntegerArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), invalidGetIntegerValue),
          (dataEntryForTests(randomStringArrayElement), invalidGetIntegerValueArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultIntegerEntry)
        assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
