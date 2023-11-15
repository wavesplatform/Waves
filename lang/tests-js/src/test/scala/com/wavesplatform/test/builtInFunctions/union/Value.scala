package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{MATCHING_NOT_EXHAUSTIVE, actualVersions, invalidFunctionError}
import utest.{Tests, test}

object Value extends JsTestBase {
  private val value                     = "value(callerTestData)"
  private val valueArgBeforeFunc        = "callerTestData.value()"
  private val invalidValue              = "value()"
  private val invalidValueArgBeforeFunc = "callerTestData.value(callerTestData)"
  private val invalidErrorValue         = invalidFunctionError("value", 1)

  val tests: Tests = Tests {
    test("RIDE-230. function value should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, value),
            (randomInt.toString, valueArgBeforeFunc),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-231. function value throw a compilation error for can't find overload") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, value, MATCHING_NOT_EXHAUSTIVE),
            (randomAddressDataArrayElement, valueArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE),
            (randomInt.toString, invalidValue, invalidErrorValue),
            (randomInt.toString, invalidValueArgBeforeFunc, invalidErrorValue),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
