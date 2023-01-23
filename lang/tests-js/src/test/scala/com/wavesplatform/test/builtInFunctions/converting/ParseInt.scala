package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomByteVectorArrayElement, randomInt, randomStringArrayElement}
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
    test("check: function parseInt compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          parseInt,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function parseInt compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          parseIntArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function parseIntValue compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          parseIntValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function parseIntValue compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          parseIntValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: parseInt Non-matching types: expected: String, actual: Int") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          parseInt,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: parseInt Non-matching types: expected: String, actual: Int (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          parseIntArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: parseIntValue Non-matching types: expected: String, actual: Int") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          parseIntValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: parseIntValue Non-matching types: expected: String, actual: Int (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          parseIntValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: Function 'parseInt' requires 1 arguments, but 2 are provided") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          invalidParseInt,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("parseInt", 1))
      }
    }

    test("compilation error: Function 'parseInt' requires 1 arguments, but 2 are provided (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          invalidParseIntArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("parseInt", 1))
      }
    }

    test("compilation error:Function 'parseIntValue' requires 1 arguments, but 2 are provided") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          invalidParseIntValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("parseIntValue", 1))
      }
    }

    test("compilation error: Function 'parseIntValue' requires 1 arguments, but 2 are provided (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          invalidParseIntValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("parseIntValue", 1))
      }
    }
  }
}
