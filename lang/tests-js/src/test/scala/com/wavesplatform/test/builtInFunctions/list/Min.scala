package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V4
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.intList
import utest.{Tests, test}

object Min extends JsTestBase {
  // min
  private val min                     = "min(callerTestData)"
  private val minArgBeforeFunc        = "callerTestData.min()"
  private val invalidMax              = "min()"
  private val invalidMaxArgBeforeFunc = "callerTestData.min(callerTestData)"
  private val minForBigInt                     = "min([callerTestData])"
  private val minForBigIntArgBeforeFunc        = "[callerTestData].min()"
  private val invalidMaxForBigInt              = "[callerTestData].min([callerTestData], [callerTestData])"

  val tests: Tests = Tests {
    test("check: min function compiles with a address data type") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, min)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: min function compiles with a address data type (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, minArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: min for BigInt function compiles with a address data type") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", minForBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: min for BigInt function compiles with a address data type (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", minForBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: min - invalid data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, min)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[Int]"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: Can't find a function overload min (argument before function) - invalid data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, minArgBeforeFunc)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[Int]"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: Can't find a function overload min - invalid function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidMax)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.invalidFunctionError("min", 1))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: Can't find a function overload min (argument before function) - invalid function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidMaxArgBeforeFunc)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.invalidFunctionError("min", 1))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: min - invalid function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidMaxForBigInt)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.invalidFunctionError("min", 1))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }

}
