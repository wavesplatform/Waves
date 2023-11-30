package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object Take extends JsTestBase {
  private val take                     = s"take(callerTestData, $randomInt)"
  private val takeArgBeforeFunction    = s"callerTestData.take($randomInt)"
  private val invalidTake              = s"take(callerTestData)"
  private val invalidTakeArgBeforeFunc = s"callerTestData.take(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("RIDE-221. function take should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, take),
            (randomStringArrayElement, takeArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-222. function take throw a compilation error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, take),
            (randomUnionArrayElement, takeArgBeforeFunction),
            (randomStringArrayElement, invalidTake),
            (randomStringArrayElement, invalidTakeArgBeforeFunc)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
