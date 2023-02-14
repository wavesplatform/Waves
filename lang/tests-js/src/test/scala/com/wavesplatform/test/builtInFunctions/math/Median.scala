package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, nonMatchingTypes, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object Median extends JsTestBase {
  private val medianInt                 = s"median([callerTestData, $randomInt, $randomInt])"
  private val medianIntArgBeforeFunc    = s"[callerTestData, $randomInt, $randomInt].median()"
  private val medianBigInt              = s"median(callerTestData)"
  private val medianBigIntArgBeforeFunc = s"callerTestData.median()"
  private val invalidMedianInt          = s"median()"
  private val medianError: String       = testData.invalidFunctionError("median", 1)

  val tests: Tests = Tests {
    test("Median functions compiles with Int") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, medianInt),
            (randomInt.toString, medianIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Median functions compiles with BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (s"[toBigInt(${randomInt.toString})]", medianBigInt),
            (s"[toBigInt(${randomInt.toString})]", medianBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("invalid median functions with Int") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, invalidMedianInt, medianError),
            (randomStringArrayElement, medianInt, nonMatchingTypes("List[Int]")),
            (randomAddressDataArrayElement, medianIntArgBeforeFunc, nonMatchingTypes("List[Int]"))
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
    }

    test("invalid median with BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, medianBigInt),
            (randomAliasDataArrayElement, medianBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
