package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V4
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.intList
import utest.{Tests, test}

object Max extends JsTestBase {
  // max
  private val max                     = "max(callerTestData)"
  private val maxArgBeforeFunc        = "callerTestData.max()"
  private val invalidMax              = "max()"
  private val invalidMaxArgBeforeFunc = "callerTestData.max(callerTestData)"
  private val maxForBigInt                     = "max([callerTestData])"
  private val maxForBigIntArgBeforeFunc        = "[callerTestData].max()"
  private val invalidMaxForBigInt              = "[callerTestData].max([callerTestData], [callerTestData])"

  val tests: Tests = Tests {
    test.apply("check: max function compiles with a address data type") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, max)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: max function compiles with a address data type (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, maxArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: max for BigInt function compiles with a address data type") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", maxForBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: max for BigInt function compiles with a address data type (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", maxForBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: max - invalid data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, max)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[Int]"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: Can't find a function overload max (argument before function) - invalid data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, maxArgBeforeFunc)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[Int]"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: Can't find a function overload max - invalid function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidMax)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.invalidFunctionError("max", 1))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: Can't find a function overload max (argument before function) - invalid function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidMaxArgBeforeFunc)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.invalidFunctionError("max", 1))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: max - invalid function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidMaxForBigInt)
        if (version == V4) {
          assertCompileErrorDApp(script, version, testData.invalidFunctionError("max", 1))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
