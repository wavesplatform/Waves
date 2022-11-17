package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V5
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomByteVectorArrayElement,
  randomAliasDataArrayElement,
  randomStringArrayElement,
  randomIssuesArrayElement,
  randomUnionArrayElement,
  randomInt
}
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
    test.apply("check: fraction Int function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, fractionInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction Int function compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, fractionIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction Int and union function compiles ") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, fractionIntAndUnion)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction Int and union function compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, fractionIntAndUnionArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction BigInt function compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", fractionBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction BigInt function compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", fractionBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction BigInt and union function compiles ") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(
          s"toBigInt(${randomInt.toString})",
          fractionBigIntAndUnion
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: fraction BigInt and union function compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", fractionBigIntAndUnionArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: invalid fraction function") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, invalidFractionFunction)
        if (version < V5) {
          assertCompileErrorDApp(script, version, fractionError)
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid fraction function (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, invalidFractionFunctionArgBeforeFunc)
        if (version < V5) {
          assertCompileErrorDApp(script, version, fractionError)
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid fraction data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomStringArrayElement, fractionInt)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid fraction data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, fractionIntArgBeforeFunc)
        if (version < V5) {
          assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
        } else {
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test.apply("compilation error: invalid fraction data BigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomStringArrayElement, fractionBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid fraction data BigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomAliasDataArrayElement, fractionBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid fraction data BigInt and unit") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomStringArrayElement, fractionBigIntAndUnion)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid fraction data BigInt and unit (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomAliasDataArrayElement, fractionBigIntAndUnionArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: fraction Int and Union invalid data") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomIssuesArrayElement, fractionIntAndUnion)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: fraction Int and Union invalid data (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, fractionIntAndUnionArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Function 'fraction' requires 3 arguments, but 4 are provided for V3, V4") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, fractionIntAndUnion)
        assertCompileErrorDApp(script, version, fractionError)
      }
    }
  }
}
