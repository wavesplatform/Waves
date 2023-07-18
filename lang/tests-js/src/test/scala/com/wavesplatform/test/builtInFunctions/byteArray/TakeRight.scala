package com.wavesplatform.test.builtInFunctions.byteArray

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object TakeRight extends JsTestBase {
  private val takeRight                     = s"takeRight(callerTestData, $randomInt)"
  private val takeRightArgBeforeFunction    = s"callerTestData.takeRight($randomInt)"
  private val invalidTakeRight              = s"takeRight(callerTestData)"
  private val invalidTakeRightArgBeforeFunc = s"callerTestData.takeRight(callerTestData, $randomInt)"
  private val invalidTakeRightNotInt        = s"takeRight(callerTestData, $randomByteVectorArrayElement)"

  val tests: Tests = Tests {
    test("RIDE-58. TakeRight function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, takeRight),
            (randomByteVectorArrayElement, takeRightArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-59. TakeRight function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, takeRight),
            (randomUnionArrayElement, takeRightArgBeforeFunction),
            (randomByteVectorArrayElement, invalidTakeRight),
            (randomByteVectorArrayElement, invalidTakeRightArgBeforeFunc),
            (randomByteVectorArrayElement, invalidTakeRightNotInt)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
