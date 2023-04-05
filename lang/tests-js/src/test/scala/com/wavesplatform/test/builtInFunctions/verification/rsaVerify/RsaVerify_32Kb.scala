package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object RsaVerify_32Kb extends JsTestBase {
  private val rsaVerify_32Kb              = s"rsaVerify_32Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_32KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_32Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_32Kb       = "rsaVerify_32Kb()"
  private val invalidRsaVerify_32KbArgBeforeFunc = "callerTestData.rsaVerify_32Kb(callerTestData)"
  private val invalidErrorRsaVerify_32Kb         = testData.invalidFunctionError("rsaVerify_32Kb", 4)

  val tests: Tests = Tests {
    test("rsaVerify_32Kb functions compiles") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, rsaVerify_32Kb),
            (randomByteVectorArrayElement, rsaVerify_32KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("invalid functions rsaVerify_32Kb") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, rsaVerify_32Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, rsaVerify_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidRsaVerify_32Kb, invalidErrorRsaVerify_32Kb),
            (randomByteVectorArrayElement, invalidRsaVerify_32KbArgBeforeFunc, invalidErrorRsaVerify_32Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("compilation error: Can't find a function rsaVerify_32Kb for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_32Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
