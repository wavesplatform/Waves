package com.wavesplatform.test.builtInFunctions.byteArray

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object DropRight extends JsTestBase {
  private val dropRight                     = s"dropRight(callerTestData, $randomInt)"
  private val dropRightArgBeforeFunction    = s"callerTestData.dropRight($randomInt)"
  private val invalidDropRight              = s"dropRight(callerTestData)"
  private val invalidDropRightNotInt        = s"dropRight(callerTestData, $randomByteVectorArrayElement)"
  private val invalidDropRightArgBeforeFunc = s"callerTestData.dropRight(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("RIDE-52. DropRight function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, dropRight),
            (randomByteVectorArrayElement, dropRightArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-53. DropRight function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, dropRight),
            (randomUnionArrayElement, dropRightArgBeforeFunction),
            (randomByteVectorArrayElement, invalidDropRight),
            (randomByteVectorArrayElement, invalidDropRightArgBeforeFunc),
            (randomByteVectorArrayElement, invalidDropRightNotInt)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
