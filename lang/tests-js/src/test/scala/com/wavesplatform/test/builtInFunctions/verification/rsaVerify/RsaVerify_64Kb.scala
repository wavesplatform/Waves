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

object RsaVerify_64Kb extends JsTestBase {
  private val rsaVerify_64Kb              = s"rsaVerify_64Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_64KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_64Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_64Kb       = "rsaVerify_64Kb()"
  private val invalidRsaVerify_64KbArgBeforeFunc = "callerTestData.rsaVerify_64Kb(callerTestData)"
  private val invalidErrorRsaVerify_64Kb         = testData.invalidFunctionError("rsaVerify_64Kb", 4)

  val tests: Tests = Tests {
    test("RIDE-245. rsaVerify_64Kb function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, rsaVerify_64Kb),
            (randomByteVectorArrayElement, rsaVerify_64KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-246. rsaVerify_64Kb function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, rsaVerify_64Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, rsaVerify_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidRsaVerify_64Kb, invalidErrorRsaVerify_64Kb),
            (randomByteVectorArrayElement, invalidRsaVerify_64KbArgBeforeFunc, invalidErrorRsaVerify_64Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-247. Can't find a function rsaVerify_64Kb for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_64Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
