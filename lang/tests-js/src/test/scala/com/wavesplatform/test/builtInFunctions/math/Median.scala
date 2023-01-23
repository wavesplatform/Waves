package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import utest.{Tests, test}

object Median extends JsTestBase {
  private val medianInt = s"median([callerTestData, $randomInt, $randomInt])"
  private val medianIntArgBeforeFunc = s"[callerTestData, $randomInt, $randomInt].median()"
  private val medianBigInt = s"median(callerTestData)"
  private val medianBigIntArgBeforeFunc = s"callerTestData.median()"
  private val invalidMedianInt = s"median()"
  private val medianError: String = testData.invalidFunctionError("median", 1)

  val tests: Tests = Tests {
    test("check: median Int function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, medianInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: median Int function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, medianIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: median BigInt function compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(s"[toBigInt(${randomInt.toString})]", medianBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: median BigInt function compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(s"[toBigInt(${randomInt.toString})]", medianBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: invalid median function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, invalidMedianInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, medianError)
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: invalid median data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, medianInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[Int]"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: invalid median data (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomAddressDataArrayElement, medianIntArgBeforeFunc)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[Int]"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: invalid median data BigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, medianBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid median data BigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomAliasDataArrayElement, medianBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }

}
