package com.wavesplatform.test.builtInFunctions.byteArray

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object Take extends JsTestBase {
  private val take                     = s"take(callerTestData, $randomInt)"
  private val takeArgBeforeFunction    = s"callerTestData.take($randomInt)"
  private val invalidTake              = s"take(callerTestData)"
  private val invalidTakeArgBeforeFunc = s"callerTestData.take(callerTestData, $randomInt)"
  private val invalidTakeNotInt        = s"take(callerTestData, $randomByteVectorArrayElement)"

  val tests: Tests = Tests {
    test("RIDE-56. Take function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, take),
            (randomByteVectorArrayElement, takeArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-57. Take function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, take),
            (randomUnionArrayElement, takeArgBeforeFunction),
            (randomByteVectorArrayElement, invalidTake),
            (randomByteVectorArrayElement, invalidTakeArgBeforeFunc),
            (randomByteVectorArrayElement, invalidTakeNotInt)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
