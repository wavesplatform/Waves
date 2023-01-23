package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import utest.{Tests, test}

object ParseBigInt extends JsTestBase {
  private val parseBigInt                                   = "parseBigInt(callerTestData)"
  private val parseBigIntArgBeforeFunc                      = "callerTestData.parseBigInt()"
  private val invalidFunctionParseBigInt                    = "parseBigInt()"
  private val invalidParseBigIntValueArgBeforeFunc          = "callerTestData.parseBigInt(callerTestData,callerTestData)"
  private val invalidFunctionErrorResultParseBigInt: String = testData.invalidFunctionError("parseBigInt", 1)

  private val parseBigIntValue                          = "parseBigIntValue(callerTestData)"
  private val parseBigIntValueArgBeforeFunc             = "callerTestData.parseBigIntValue()"
  private val invalidFunctionParseBigIntValue           = "parseBigIntValue()"
  private val invalidParseBigIntValueValueArgBeforeFunc = "callerTestData.parseBigIntValue(callerTestData,callerTestData)"

  private val invalidFunctionErrorResultParseBigIntValue: String = testData.invalidFunctionError("parseBigIntValue", 1)

  val tests: Tests = Tests {
    test("check: function parseBigInt compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          parseBigInt
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function parseBigInt compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          parseBigIntArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: Can't find function parseBigInt (argument before function)") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          parseBigIntArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.UNDEFINED_TYPE)
      }
    }

    test("compilation error: invalid function parseBigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFunctionParseBigInt
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorResultParseBigInt)
      }
    }

    test("compilation error: invalid function parseBigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidParseBigIntValueArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorResultParseBigInt)
      }
    }

    test("compilation error: invalid data for function parseBigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          parseBigInt
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: invalid data for function parseBigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          parseBigIntArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("check: function parseBigIntValue compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          parseBigIntValue
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function parseBigIntValue compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          parseBigIntValueArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: Can't find function parseBigIntValue (argument before function)") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          parseBigIntValueArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.UNDEFINED_TYPE)
      }
    }

    test("compilation error: invalid function parseBigIntValue") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFunctionParseBigIntValue
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorResultParseBigIntValue)
      }
    }

    test("compilation error: invalid function parseBigIntValue") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidParseBigIntValueValueArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorResultParseBigIntValue)
      }
    }

    test("compilation error: invalid data for function parseBigIntValue") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          parseBigIntValue
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: invalid data for function parseBigIntValue (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          parseBigIntValueArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }
  }
}
