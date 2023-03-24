package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomInt,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object ToInt extends JsTestBase {
  private val toInt                        = "toInt(callerTestData)"
  private val toIntArgBeforeFunc           = "callerTestData.toInt()"
  private val toIntOnIndex                 = s"toInt(callerTestData, $randomInt)"
  private val toIntOnIndexArgBeforeFunc    = s"callerTestData.toInt($randomInt)"
  private val invalidFunctionParseInt      = "toInt()"
  private val invalidParseIntArgBeforeFunc = s"callerTestData.toInt(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test(" Functions toInt compiles with ByteVector") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, toInt),
            (randomByteVectorArrayElement, toIntOnIndex),
            (randomByteVectorArrayElement, toIntArgBeforeFunc),
            (randomByteVectorArrayElement, toIntOnIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test(" Functions toInt compiles with BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (s"toBigInt($randomInt)", toInt),
            (s"toBigInt($randomInt)", toIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test(" Functions toInt negative tests") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomDigestAlgorithmTypeArrayElement, toInt),
            (randomStringArrayElement, toIntOnIndex),
            (randomUnionArrayElement, toIntOnIndexArgBeforeFunc),
            (randomInt.toString, toIntArgBeforeFunc),
            (s"toBigInt($randomInt)", invalidFunctionParseInt),
            (randomByteVectorArrayElement, invalidParseIntArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
