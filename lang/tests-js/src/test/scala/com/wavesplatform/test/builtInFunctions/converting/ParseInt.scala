package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultIntegerEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object ParseInt extends JsTestBase {
  // parseInt
  private val parseInt                     = "parseInt(callerTestData)"
  private val parseIntArgBeforeFunc        = "callerTestData.parseInt()"
  private val invalidParseInt              = "parseInt()"
  private val invalidParseIntArgBeforeFunc = "callerTestData.parseInt(callerTestData)"

  // parseIntValue
  private val parseIntValue                     = "parseIntValue(callerTestData)"
  private val parseIntValueArgBeforeFunc        = "callerTestData.parseIntValue()"
  private val invalidParseIntValue              = "parseIntValue()"
  private val invalidParseIntValueArgBeforeFunc = "callerTestData.parseIntValue(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-68. ParseInt function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, parseInt),
            (randomStringArrayElement, parseIntArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-69. ParseInt function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, parseInt, nonMatchingTypes("String")),
            (randomAliasDataArrayElement, parseIntArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, invalidParseInt, invalidFunctionError("parseInt", 1)),
            (randomStringArrayElement, invalidParseIntArgBeforeFunc, invalidFunctionError("parseInt", 1))
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-70. parseIntValue function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, parseIntValue),
            (randomStringArrayElement, parseIntValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-71. parseIntValue function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, parseIntValue, nonMatchingTypes("String")),
            (randomAliasDataArrayElement, parseIntValueArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, invalidParseIntValue, invalidFunctionError("parseIntValue", 1)),
            (randomStringArrayElement, invalidParseIntValueArgBeforeFunc, invalidFunctionError("parseIntValue", 1))
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
