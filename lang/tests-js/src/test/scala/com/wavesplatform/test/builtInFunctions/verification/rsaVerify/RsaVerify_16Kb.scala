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

object RsaVerify_16Kb extends JsTestBase {
  private val rsaVerify_16Kb              = s"rsaVerify_16Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_16KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_16Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_16Kb       = "rsaVerify_16Kb()"
  private val invalidRsaVerify_16KbArgBeforeFunc = "callerTestData.rsaVerify_16Kb(callerTestData)"
  private val invalidErrorRsaVerify_16Kb         = testData.invalidFunctionError("rsaVerify_16Kb", 4)

  val tests: Tests = Tests {
    test("RIDE-239. rsaVerify_16Kb function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, rsaVerify_16Kb),
            (randomByteVectorArrayElement, rsaVerify_16KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-240. rsaVerify_16Kb function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, rsaVerify_16Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, rsaVerify_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidRsaVerify_16Kb, invalidErrorRsaVerify_16Kb),
            (randomByteVectorArrayElement, invalidRsaVerify_16KbArgBeforeFunc, invalidErrorRsaVerify_16Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-241. Can't find a function makeString_2C for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_16Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
