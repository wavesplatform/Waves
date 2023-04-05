package com.wavesplatform.test.builtInFunctions.dataTransaction

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{dataEntryForTests, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.*
import utest.{Tests, test}

object GetBinary extends JsTestBase {
  // getBinaryKey
  private val getBinaryKey              = s"getBinary(callerTestData, \"key\")"
  private val getBinaryKeyArgBeforeFunc = s"callerTestData.getBinary(\"key\")"
  // getBinaryIndex
  private val getBinaryIndex              = s"getBinary(callerTestData, $randomInt)"
  private val getBinaryIndexArgBeforeFunc = s"callerTestData.getBinary($randomInt)"
  // getBinaryValueKey
  private val getBinaryValueKey              = s"getBinaryValue(callerTestData, \"key\")"
  private val getBinaryValueKeyArgBeforeFunc = s"callerTestData.getBinaryValue(\"key\")"
  // getBinaryValueIndex
  private val getBinaryValueIndex              = s"getBinaryValue(callerTestData, $randomInt)"
  private val getBinaryValueIndexArgBeforeFunc = s"callerTestData.getBinaryValue($randomInt)"

  // invalid getBinary
  private val invalidGetBinaryKey           = s"getBinary()"
  private val invalidGetBinaryArgBeforeFunc = s"callerTestData.getBinary()"
  // invalid getBinaryValue
  private val invalidGetBinaryValue              = s"getBinaryValue()"
  private val invalidGetBinaryValueArgBeforeFunc = s"callerTestData.getBinaryValue()"

  val tests: Tests = Tests {
    test(" Functions getBinary dataTransaction compiles for versions V4 and more") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, binary) <- Seq(
            (binaryEntryForTests, getBinaryKey),
            (integerEntryForTests, getBinaryKey),
            (stringEntryForTests, getBinaryKey),
            (booleanEntryForTests, getBinaryKey),
            (binaryEntryForTests, getBinaryKeyArgBeforeFunc),
            (integerEntryForTests, getBinaryKeyArgBeforeFunc),
            (stringEntryForTests, getBinaryKeyArgBeforeFunc),
            (booleanEntryForTests, getBinaryKeyArgBeforeFunc),
            (binaryEntryForTests, getBinaryIndex),
            (integerEntryForTests, getBinaryIndex),
            (stringEntryForTests, getBinaryIndex),
            (booleanEntryForTests, getBinaryIndex),
            (binaryEntryForTests, getBinaryIndexArgBeforeFunc),
            (integerEntryForTests, getBinaryIndexArgBeforeFunc),
            (stringEntryForTests, getBinaryIndexArgBeforeFunc),
            (booleanEntryForTests, getBinaryIndexArgBeforeFunc),
            (binaryEntryForTests, getBinaryValueKey),
            (integerEntryForTests, getBinaryValueKey),
            (stringEntryForTests, getBinaryValueKey),
            (booleanEntryForTests, getBinaryValueKey),
            (binaryEntryForTests, getBinaryValueKeyArgBeforeFunc),
            (integerEntryForTests, getBinaryValueKeyArgBeforeFunc),
            (stringEntryForTests, getBinaryValueKeyArgBeforeFunc),
            (booleanEntryForTests, getBinaryValueKeyArgBeforeFunc),
            (binaryEntryForTests, getBinaryValueIndex),
            (integerEntryForTests, getBinaryValueIndex),
            (stringEntryForTests, getBinaryValueIndex),
            (booleanEntryForTests, getBinaryValueIndex),
            (binaryEntryForTests, getBinaryValueIndexArgBeforeFunc),
            (integerEntryForTests, getBinaryValueIndexArgBeforeFunc),
            (stringEntryForTests, getBinaryValueIndexArgBeforeFunc),
            (booleanEntryForTests, getBinaryValueIndexArgBeforeFunc),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test(" Functions getBinary dataTransaction compiles for versions V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      for (
        (data, binary) <- Seq(
          (dataEntryForTests(randomStringArrayElement), getBinaryKey),
          (dataEntryForTests(randomStringArrayElement), getBinaryKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getBinaryIndex),
          (dataEntryForTests(randomStringArrayElement), getBinaryIndexArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getBinaryValueKey),
          (dataEntryForTests(randomStringArrayElement), getBinaryValueKeyArgBeforeFunc),
          (dataEntryForTests(randomStringArrayElement), getBinaryValueIndex),
          (dataEntryForTests(randomStringArrayElement), getBinaryValueIndexArgBeforeFunc)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
        assertCompileSuccessDApp(script, V3)
      }
    }

    test(" Can't find getBinary functions overload for V4 and more") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, binary) <- Seq(
            (randomInt.toString, getBinaryKey),
            (randomBoolean.toString, getBinaryKeyArgBeforeFunc),
            (randomInt.toString, getBinaryIndex),
            (randomBoolean.toString, getBinaryIndexArgBeforeFunc),
            (randomInt.toString, getBinaryValueKey),
            (randomBoolean.toString, getBinaryValueKeyArgBeforeFunc),
            (randomInt.toString, getBinaryValueIndex),
            (randomBoolean.toString, getBinaryValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test(" Can't find getBinary functions overload for V3") {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
        for (
          (data, binary) <- Seq(
            (randomInt.toString, getBinaryKey),
            (randomBoolean.toString, getBinaryKeyArgBeforeFunc),
            (randomInt.toString, getBinaryIndex),
            (randomBoolean.toString, getBinaryIndexArgBeforeFunc),
            (randomInt.toString, getBinaryValueKey),
            (randomBoolean.toString, getBinaryValueKeyArgBeforeFunc),
            (randomInt.toString, getBinaryValueIndex),
            (randomBoolean.toString, getBinaryValueIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
    }

    test(" Invalid getBinary functions for V4 and more") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, binary) <- Seq(
            (integerEntryForTests, invalidGetBinaryKey),
            (binaryEntryForTests, invalidGetBinaryArgBeforeFunc),
            (integerEntryForTests, invalidGetBinaryValue),
            (binaryEntryForTests, invalidGetBinaryValueArgBeforeFunc),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test(" Invalid getBinary functions for V3") {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
        for (
          (data, binary) <- Seq(
            (dataEntryForTests(randomStringArrayElement), invalidGetBinaryKey),
            (dataEntryForTests(randomStringArrayElement), invalidGetBinaryArgBeforeFunc),
            (dataEntryForTests(randomStringArrayElement), invalidGetBinaryValue),
            (dataEntryForTests(randomStringArrayElement), invalidGetBinaryValueArgBeforeFunc),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, V3, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
    }
  }
}
