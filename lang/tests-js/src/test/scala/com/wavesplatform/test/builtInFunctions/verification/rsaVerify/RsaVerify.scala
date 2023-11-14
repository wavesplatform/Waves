package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object RsaVerify extends JsTestBase {
  private val rsaVerify                     = s"rsaVerify($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerifyArgBeforeFunc        = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify              = s"rsaVerify()"
  private val invalidRsaVerifyArgBeforeFunc = s"callerTestData.rsaVerify(callerTestData)"
  private val invalidErrorRsaVerify         = invalidFunctionError("rsaVerify", 4)

  val tests: Tests = Tests {
    test("RIDE-237. function rsaVerify should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, rsaVerify),
            (randomByteVectorArrayElement, rsaVerifyArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-238. function rsaVerify throw a compilation error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, rsaVerify, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, rsaVerifyArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidRsaVerify, invalidErrorRsaVerify),
            (randomByteVectorArrayElement, invalidRsaVerifyArgBeforeFunc, invalidErrorRsaVerify)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
