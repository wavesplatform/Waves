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
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, nonMatchingTypes}
import utest.{Tests, test}

object Log extends JsTestBase {
  private val union: String          = randomUnionArrayElement
  private val logInt                 = s"log(callerTestData, $randomInt, $randomInt, 4, 2, $union)"
  private val logIntArgBeforeFunc    = s"callerTestData.log($randomInt, $randomInt, 4, 2, $union)"
  private val logBigInt              = s"log(callerTestData, 6, callerTestData, $randomInt, 2, $union)"
  private val logBigIntArgBeforeFunc = s"callerTestData.log(6, callerTestData, $randomInt, 2, $union)"
  private val invalidLogInt              = s"log(callerTestData, 10, $union)"
  private val invalidLogIntArgBeforeFunc = s"callerTestData.log(10, $union)"
  private val logError: String           = testData.invalidFunctionError("log", 6)

  val tests: Tests = Tests {
    test("RIDE-177. Log function should compile with Int") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, logInt),
            (randomInt.toString, logIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-178. Log function should compile with BigInt for Ride V5, V6") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (s"toBigInt(${randomInt.toString})", logBigInt),
            (s"toBigInt(${randomInt.toString})", logBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-179. Log function should throw an error for invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function, error) <- Seq(
            (randomStringArrayElement, logInt, nonMatchingTypes("Int")),
            (randomAddressDataArrayElement, logIntArgBeforeFunc, nonMatchingTypes("Int")),
            (randomInt.toString, invalidLogInt, logError),
            (randomInt.toString, invalidLogIntArgBeforeFunc, logError)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
    }

    test("RIDE-180. Log function should throw an error for invalid data BigInt - Ride V5, V6") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, logBigInt),
            (randomAliasDataArrayElement, logBigIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
