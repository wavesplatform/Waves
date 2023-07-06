package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object RsaVerify_128Kb extends JsTestBase {
  private val rsaVerify_128Kb              = s"rsaVerify_128Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_128KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_128Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_128Kb       = "rsaVerify_128Kb()"
  private val invalidRsaVerify_128KbArgBeforeFunc = "callerTestData.rsaVerify_128Kb(callerTestData)"
  private val invalidErrorRsaVerify_128Kb         = testData.invalidFunctionError("rsaVerify_128Kb", 4)

  val tests: Tests = Tests {
    test("rsaVerify_128Kb functions compiles") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, rsaVerify_128Kb),
            (randomByteVectorArrayElement, rsaVerify_128KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("invalid functions rsaVerify_128Kb") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, rsaVerify_128Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, rsaVerify_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidRsaVerify_128Kb, invalidErrorRsaVerify_128Kb),
            (randomByteVectorArrayElement, invalidRsaVerify_128KbArgBeforeFunc, invalidErrorRsaVerify_128Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("compilation error: Can't find a function rsaVerify_128Kb for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_128Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
