package com.wavesplatform.test.builtInFunctions.byteArray

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object Size extends JsTestBase {
  private val size                     = s"size(callerTestData)"
  private val sizeArgBeforeFunction    = s"callerTestData.size()"
  private val invalidSize              = s"size()"
  private val invalidSizeArgBeforeFunc = s"callerTestData.size(callerTestData, callerTestData)"

  val tests: Tests = Tests {
    test.apply("check: function size compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomByteVectorArrayElement,
          size,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function size compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomByteVectorArrayElement,
          sizeArgBeforeFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          size,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomUnionArrayElement,
          sizeArgBeforeFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function size Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomByteVectorArrayElement,
          invalidSize,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function size Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomByteVectorArrayElement,
          invalidSizeArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
