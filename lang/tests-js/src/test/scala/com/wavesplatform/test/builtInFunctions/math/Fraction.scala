package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomInt,
  randomIssuesArrayElement,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, nonMatchingTypes, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object Fraction extends JsTestBase {
  private var union: String                    = randomUnionArrayElement
  private val fractionInt                      = s"fraction(callerTestData, $randomInt, $randomInt)"
  private val fractionIntArgBeforeFunc         = s"callerTestData.fraction($randomInt, $randomInt)"
  private val fractionIntAndUnion              = s"fraction(callerTestData, $randomInt, $randomInt, $union)"
  private val fractionIntAndUnionArgBeforeFunc = s"callerTestData.fraction($randomInt, $randomInt, $union)"

  union = randomUnionArrayElement
  private val fractionBigInt                      = s"fraction(callerTestData, callerTestData, callerTestData)"
  private val fractionBigIntArgBeforeFunc         = s"callerTestData.fraction(callerTestData, callerTestData)"
  private val fractionBigIntAndUnion              = s"fraction(callerTestData, callerTestData, callerTestData, $union)"
  private val fractionBigIntAndUnionArgBeforeFunc = s"callerTestData.fraction(callerTestData, callerTestData, $union)"

  private val invalidFractionFunction              = s"fraction(callerTestData)"
  private val invalidFractionFunctionArgBeforeFunc = s"callerTestData.fraction()"
  private val fractionError: String                = testData.invalidFunctionError("fraction", 3)

  val tests: Tests = Tests {
    test("Fraction functions compiles with Int") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, fractionInt),
            (randomInt.toString, fractionIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Fraction functions compiles with Int and union") {
      for (version <- versionsSupportingTheNewFeatures) {
        for (
          (data, function, dataType) <- Seq(
            (randomInt.toString, fractionIntAndUnion, "Int"),
            (randomInt.toString, fractionIntAndUnionArgBeforeFunc, "Int"),
            (s"toBigInt(${randomInt.toString})", fractionBigInt, "BigInt"),
            (s"toBigInt(${randomInt.toString})", fractionBigIntArgBeforeFunc, "BigInt"),
            (s"toBigInt(${randomInt.toString})", fractionBigIntAndUnion, "BigInt"),
            (s"toBigInt(${randomInt.toString})", fractionBigIntAndUnionArgBeforeFunc, "BigInt")
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation error for Fraction with Int") {
      for (version <- actualVersions) {
        for (
          (data, function, dataType, error) <- Seq(
            (randomInt.toString, invalidFractionFunction, "Int", fractionError),
            (randomInt.toString, invalidFractionFunctionArgBeforeFunc, "Int", fractionError),
            (randomStringArrayElement, fractionInt, "Int", nonMatchingTypes("Int")),
            (randomAddressDataArrayElement, fractionIntArgBeforeFunc, "Int", nonMatchingTypes("Int"))
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          if (version < V5) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
    }

    test("compilation error: invalid fraction data BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        for (
          (data, function, dataType, error) <- Seq(
            (randomStringArrayElement, fractionBigInt, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomAliasDataArrayElement, fractionBigIntArgBeforeFunc, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomStringArrayElement, fractionBigIntAndUnion, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomAliasDataArrayElement, fractionBigIntAndUnionArgBeforeFunc, "BigInt", CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomIssuesArrayElement, fractionIntAndUnion, "Int", CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomIssuesArrayElement, fractionIntAndUnionArgBeforeFunc, "Int", CANT_FIND_A_FUNCTION_OVERLOAD)
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("compilation error: Function 'fraction' requires 3 arguments, but 4 are provided for V3, V4") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, fractionIntAndUnion)
        assertCompileErrorDApp(script, version, fractionError)
      }
    }
  }
}
