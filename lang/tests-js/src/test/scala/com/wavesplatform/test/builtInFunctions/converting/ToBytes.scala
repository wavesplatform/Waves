package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomBoolean, randomInt, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_A_FUNCTION_OVERLOAD,
  GreaterV3ResultBinaryEntry,
  actualVersions,
  rideV3Result,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object ToBytes extends JsTestBase {
  // toBytes
  private val toBytes                     = "toBytes(callerTestData)"
  private val toBytesArgBeforeFunc        = "callerTestData.toBytes()"
  private val invalidToBytes              = "toBytes()"
  private val invalidToBytesArgBeforeFunc = "callerTestData.toBytes(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-74. toBytes function should compile for valid values int, string, boolean") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, toBytes),
            (randomInt.toString, toBytes),
            (randomBoolean.toString, toBytes),
            (randomStringArrayElement, toBytesArgBeforeFunc),
            (randomInt.toString, toBytesArgBeforeFunc),
            (randomBoolean.toString, toBytesArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-75. toBytes function should compile with bigInt for V5, V6 versions") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (s"toBigInt($randomInt)", toBytes),
            (s"toBigInt($randomInt)", toBytesArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-76. toBytes function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomAddressDataArrayElement, toBytes, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomUnionArrayElement, toBytesArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomInt.toString, invalidToBytes, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomStringArrayElement, invalidToBytesArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
