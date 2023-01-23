package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object TakeRight extends JsTestBase {
  private val takeRight                     = s"takeRight(callerTestData, $randomInt)"
  private val takeRightArgBeforeFunction    = s"callerTestData.takeRight($randomInt)"
  private val invalidTakeRight              = s"takeRight(callerTestData)"
  private val invalidTakeRightArgBeforeFunc = s"callerTestData.takeRight(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("check: function takeRight compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          takeRight,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function takeRight compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          takeRightArgBeforeFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: Can't find a function overload, invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          takeRight,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload, invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomUnionArrayElement,
          takeRightArgBeforeFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid function takeRight Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          invalidTakeRight,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid function takeRight Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomStringArrayElement,
          invalidTakeRightArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
