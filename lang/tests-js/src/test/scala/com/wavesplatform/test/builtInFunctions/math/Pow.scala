package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Pow extends JsTestBase {
  private val union                      = randomUnionArrayElement
  private val powInt                     = s"pow(callerTestData, 6, $randomInt, 4, $randomInt, $union)"
  private val powIntArgBeforeFunc        = s"callerTestData.pow(6, $randomInt, $randomInt, 2, $union)"
  private val powBigInt                  = s"pow(callerTestData, 6, callerTestData, 4, 2, $union)"
  private val powBigIntArgBeforeFunc     = s"callerTestData.pow($randomInt, callerTestData, 4, 2, $union)"
  private val invalidPowInt              = s"pow()"
  private val powError: String = testData.invalidFunctionError("pow", 6)

  val tests: Tests = Tests {
    test("check: pow Int function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, powInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: pow Int function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, powIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: pow BigInt function compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", powBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: pow BigInt function compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", powBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: invalid pow function") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, invalidPowInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, powError)
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: invalid pow data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, powInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: invalid pow data (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomAddressDataArrayElement, powIntArgBeforeFunc)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("compilation error: invalid pow data BigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, powBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid pow data BigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomAliasDataArrayElement, powBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
