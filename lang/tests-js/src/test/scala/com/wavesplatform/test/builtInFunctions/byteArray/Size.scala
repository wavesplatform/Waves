package com.wavesplatform.test.builtInFunctions.byteArray

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultIntegerEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object Size extends JsTestBase {
  private val size                     = s"size(callerTestData)"
  private val sizeArgBeforeFunction    = s"callerTestData.size()"
  private val invalidSize              = s"size()"
  private val invalidSizeArgBeforeFunc = s"callerTestData.size(callerTestData, callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-54. Size function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, size),
            (randomByteVectorArrayElement, sizeArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-55. Size function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, size),
            (randomUnionArrayElement, sizeArgBeforeFunction),
            (randomByteVectorArrayElement, invalidSize),
            (randomByteVectorArrayElement, invalidSizeArgBeforeFunc)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
