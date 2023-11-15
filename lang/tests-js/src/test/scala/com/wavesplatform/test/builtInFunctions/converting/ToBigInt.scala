package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomBoolean, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object ToBigInt extends JsTestBase {
  private val toBigInt                        = "toBigInt(callerTestData)"
  private val toBigIntArgBeforeFunc           = "callerTestData.toBigInt()"
  private val toBigIntOnIndex                 = s"toBigInt(callerTestData, 1, $randomInt)"
  private val toBigIntOnIndexArgBeforeFunc    = s"callerTestData.toBigInt(9, $randomInt)"
  private val invalidFunctionParseBigInt      = "toBigInt()"
  private val invalidParseBigIntArgBeforeFunc = s"callerTestData.toBigInt(callerTestData, 123, $randomInt)"

  val tests: Tests = Tests {
    test("RIDE-72. ToBigInt function should compile for valid values") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, toBigInt),
            (randomByteVectorArrayElement, toBigInt),
            (randomInt.toString, toBigIntArgBeforeFunc),
            (randomByteVectorArrayElement, toBigIntArgBeforeFunc),
            (randomByteVectorArrayElement, toBigIntOnIndex),
            (randomByteVectorArrayElement, toBigIntOnIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-73. ToBigInt function throws an error for invalid values") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function, error) <- Seq(
            (randomBoolean.toString, toBigIntArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomUnionArrayElement, toBigIntArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomInt.toString, invalidFunctionParseBigInt, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomDigestAlgorithmTypeArrayElement, toBigIntOnIndex, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomBoolean.toString, toBigIntOnIndexArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomInt.toString, toBigIntOnIndexArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomByteVectorArrayElement, invalidFunctionParseBigInt, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomByteVectorArrayElement, invalidParseBigIntArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
