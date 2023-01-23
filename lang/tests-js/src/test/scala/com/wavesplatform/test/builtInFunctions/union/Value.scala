package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object Value extends JsTestBase {
  // value
  private val value                     = "value(callerTestData)"
  private val valueArgBeforeFunc        = "callerTestData.value()"
  private val invalidValue              = "value()"
  private val invalidValueArgBeforeFunc = "callerTestData.value(callerTestData)"
  private val invalidErrorValue         = testData.invalidFunctionError("value", 1)

  val tests: Tests = Tests {
    test("check: value function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, value)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: value function compiles(argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, valueArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: value - Matching not exhaustive") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, value)
        assertCompileErrorDApp(script, version, testData.MATCHING_NOT_EXHAUSTIVE)
      }
    }

    test("compilation error: value - Matching not exhaustive (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, valueArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.MATCHING_NOT_EXHAUSTIVE)
      }
    }

    test("compilation error: Can't find a function overload value") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidValue)
        assertCompileErrorDApp(script, version, invalidErrorValue)
      }
    }

    test("compilation error: Can't find a function overload value (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidValueArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorValue)
      }
    }
  }
}
