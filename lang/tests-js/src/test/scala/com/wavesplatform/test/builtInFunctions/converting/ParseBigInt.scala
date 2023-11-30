package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{UNDEFINED_TYPE, nonMatchingTypes, oldVersions, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object ParseBigInt extends JsTestBase {
  private val parseBigInt                                   = "parseBigInt(callerTestData)"
  private val parseBigIntArgBeforeFunc                      = "callerTestData.parseBigInt()"
  private val invalidFunctionParseBigInt                    = "parseBigInt()"
  private val invalidValueParseBigIntArgBeforeFunc          = "callerTestData.parseBigInt(callerTestData, callerTestData)"
  private val invalidFunctionErrorResultParseBigInt: String = testData.invalidFunctionError("parseBigInt", 1)

  private val parseBigIntValue                          = "parseBigIntValue(callerTestData)"
  private val parseBigIntValueArgBeforeFunc             = "callerTestData.parseBigIntValue()"
  private val invalidFunctionParseBigIntValue           = "parseBigIntValue()"
  private val invalidValueParseBigIntValueArgBeforeFunc = "callerTestData.parseBigIntValue(callerTestData,callerTestData)"

  private val invalidFunctionErrorResultParseBigIntValue: String = testData.invalidFunctionError("parseBigIntValue", 1)

  val tests: Tests = Tests {
    test("RIDE-62. ParseBigInt function should compile for valid values") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, parseBigInt),
            (randomStringArrayElement, parseBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-63. ParseBigInt function throws an error for invalid values") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function, error) <- Seq(
            (randomStringArrayElement, parseBigInt, UNDEFINED_TYPE),
            (randomStringArrayElement, parseBigIntArgBeforeFunc, UNDEFINED_TYPE),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-64. Function parseBigInt should throw an error for invalid functions") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, parseBigInt, nonMatchingTypes("String")),
            (randomBoolean.toString, parseBigIntArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, invalidFunctionParseBigInt, invalidFunctionErrorResultParseBigInt),
            (randomStringArrayElement, invalidValueParseBigIntArgBeforeFunc, invalidFunctionErrorResultParseBigInt),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-65. ParseBigIntValue function should compile for valid values") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, parseBigIntValue),
            (randomStringArrayElement, parseBigIntValueArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-66. ParseBigIntValue function throws an error for invalid values") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function, error) <- Seq(
            (randomStringArrayElement, parseBigIntValue, UNDEFINED_TYPE),
            (randomStringArrayElement, parseBigIntValueArgBeforeFunc, UNDEFINED_TYPE),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-67. Function parseBigIntValue should throw an error for invalid functions") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, parseBigIntValue, nonMatchingTypes("String")),
            (randomBoolean.toString, parseBigIntValueArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, invalidFunctionParseBigIntValue, invalidFunctionErrorResultParseBigIntValue),
            (randomStringArrayElement, invalidValueParseBigIntValueArgBeforeFunc, invalidFunctionErrorResultParseBigIntValue),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
