package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultIntegerEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object Size extends JsTestBase {
  private val size                     = s"size(callerTestData)"
  private val sizeArgBeforeFunction    = s"callerTestData.size()"
  private val invalidSize              = s"size()"
  private val invalidSizeArgBeforeFunc = s"callerTestData.size(callerTestData, callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-219. function size should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, size),
            (randomStringArrayElement, sizeArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)

        }
      }
    }

    test("RIDE-220. function size throw a compilation error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, size),
            (randomUnionArrayElement, sizeArgBeforeFunction),
            (randomStringArrayElement, invalidSize),
            (randomStringArrayElement, invalidSizeArgBeforeFunc)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
