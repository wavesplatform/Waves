package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object ToInt extends JsTestBase {
  private val toInt                        = "toInt(callerTestData)"
  private val toIntArgBeforeFunc           = "callerTestData.toInt()"
  private val toIntOnIndex                 = s"toInt(callerTestData, $randomInt)"
  private val toIntOnIndexArgBeforeFunc    = s"callerTestData.toInt($randomInt)"
  private val invalidFunctionParseInt      = "toInt()"
  private val invalidParseIntArgBeforeFunc = s"callerTestData.toInt(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("check: The toInt function is compiled with ByteVector") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: The toInt function is compiled with BigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", toInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: The toInt function is compiled with ByteVector (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: The toInt function is compiled with BigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", toIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: The toIntOnIndex function is compiled with ByteVector") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toIntOnIndex)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: The toIntOnIndex function is compiled with ByteVector (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toIntOnIndexArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: invalid data for function toIntOnIndex with BigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(s"toBigInt($randomInt)", toIntOnIndexArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid data for function toInt") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, toInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid data for function toInt (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, toIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid function toInt") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidFunctionParseInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid function toIntAtIndex (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, invalidParseIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid data for function toIntOnIndex") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, toIntOnIndex)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid data for function toIntOnIndex (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, toIntOnIndexArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid function toIntOnIndex") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidFunctionParseInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: invalid function toIntOnIndex (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, invalidParseIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
