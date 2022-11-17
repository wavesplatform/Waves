package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomStringArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object Take extends JsTestBase {
  private val take                     = s"take(callerTestData, $randomInt)"
  private val takeArgBeforeFunction    = s"callerTestData.take($randomInt)"
  private val invalidTake              = s"take(callerTestData)"
  private val invalidTakeArgBeforeFunc = s"callerTestData.take(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test.apply("check: function take compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          take,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function take compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          takeArgBeforeFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          take,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomUnionArrayElement,
          takeArgBeforeFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function take Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          invalidTake,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function take Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          invalidTakeArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
