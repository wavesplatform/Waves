package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomInt,
  randomUnionArrayElement
}
import utest.{Tests, test}

object ToBigInt extends JsTestBase {
  private val toBigInt                        = "toBigInt(callerTestData)"
  private val toBigIntArgBeforeFunc           = "callerTestData.toBigInt()"
  private val toBigIntOnIndex                 = s"toBigInt(callerTestData, 1, $randomInt)"
  private val toBigIntOnIndexArgBeforeFunc    = s"callerTestData.toBigInt(9, $randomInt)"
  private val invalidFunctionParseBigInt      = "toBigInt()"
  private val invalidParseBigIntArgBeforeFunc = s"callerTestData.toBigInt(callerTestData, 123, $randomInt)"

  val tests: Tests = Tests {
    test.apply("check: The toBigInt function is compiled with ByteVector") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: The toBigInt function is compiled with Integer") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, toBigInt)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: The toBigInt function is compiled with ByteVector (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: The toBigInt function is compiled with Integer (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, toBigIntArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: invalid data for function toBigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, toBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid data for function toBigInt (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, toBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function toBigInt") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidFunctionParseBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function toBigIntAtIndex (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, invalidParseBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("check: The toBigIntOnIndex function is compiled with ByteVector") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toBigIntOnIndex)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: The toBigIntAtIndex function is compiled with Integer") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toBigIntOnIndex)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: The toBigIntOnIndex function is compiled with ByteVector (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toBigIntOnIndexArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: The toBigIntOnIndex function is compiled with Integer (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, toBigIntOnIndexArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: invalid data for function toBigIntOnIndex") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, toBigIntOnIndex)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid data for function toBigIntOnIndex (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, toBigIntOnIndexArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function toBigIntOnIndex") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidFunctionParseBigInt)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: invalid function toBigIntOnIndex (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, invalidParseBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
