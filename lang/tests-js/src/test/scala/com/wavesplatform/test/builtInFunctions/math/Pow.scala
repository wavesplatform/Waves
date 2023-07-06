package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomInt,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_A_FUNCTION_OVERLOAD,
  actualVersionsWithoutV3,
  nonMatchingTypes,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object Pow extends JsTestBase {
  private val union                  = randomUnionArrayElement
  private val powInt                 = s"pow(callerTestData, 6, $randomInt, 4, $randomInt, $union)"
  private val powIntArgBeforeFunc    = s"callerTestData.pow(6, $randomInt, $randomInt, 2, $union)"
  private val powBigInt              = s"pow(callerTestData, 6, callerTestData, 4, 2, $union)"
  private val powBigIntArgBeforeFunc = s"callerTestData.pow($randomInt, callerTestData, 4, 2, $union)"
  private val invalidPowInt          = s"pow()"
  private val powError: String       = testData.invalidFunctionError("pow", 6)

  val tests: Tests = Tests {
    test("Pow functions compiles with Int") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, powInt),
            (randomInt.toString, powIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Pow functions compiles with BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (s"toBigInt(${randomInt.toString})", powBigInt),
            (s"toBigInt(${randomInt.toString})", powBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Invalid data for pow functions with Int") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, invalidPowInt, powError),
            (randomStringArrayElement, powInt, nonMatchingTypes("Int")),
            (randomAddressDataArrayElement, powIntArgBeforeFunc, nonMatchingTypes("Int"))
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

    test("Invalid data for pow functions with BigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, powBigInt),
            (randomAliasDataArrayElement, powBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
