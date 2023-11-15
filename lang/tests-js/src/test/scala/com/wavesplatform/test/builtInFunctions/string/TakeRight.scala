package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object TakeRight extends JsTestBase {
  private val takeRight                     = s"takeRight(callerTestData, $randomInt)"
  private val takeRightArgBeforeFunction    = s"callerTestData.takeRight($randomInt)"
  private val invalidTakeRight              = s"takeRight(callerTestData)"
  private val invalidTakeRightArgBeforeFunc = s"callerTestData.takeRight(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("RIDE-223. function takeRight should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, takeRight),
            (randomStringArrayElement, takeRightArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)

        }
      }
    }

    test("RIDE-224. function takeRight throw a compilation error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, takeRight),
            (randomUnionArrayElement, takeRightArgBeforeFunction),
            (randomStringArrayElement, invalidTakeRight),
            (randomStringArrayElement, invalidTakeRightArgBeforeFunc)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
