package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, nonMatchingTypes}
import utest.{Tests, test}

object SigVerify extends JsTestBase {
  private val sigVerify                     = s"sigVerify(callerTestData, callerTestData, callerTestData)"
  private val sigVerifyArgBeforeFunc        = s"callerTestData.sigVerify(callerTestData, callerTestData)"
  private val invalidSigVerify              = s"sigVerify()"
  private val invalidSigVerifyArgBeforeFunc = s"callerTestData.sigVerify(callerTestData)"
  private val invalidErrorSigVerify         = testData.invalidFunctionError("sigVerify", 3)

  val tests: Tests = Tests {
    test("sigVerify functions compiles") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sigVerify),
            (randomByteVectorArrayElement, sigVerifyArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("invalid rsaVerify functions") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sigVerify, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sigVerifyArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSigVerify, invalidErrorSigVerify),
            (randomByteVectorArrayElement, invalidSigVerifyArgBeforeFunc, invalidErrorSigVerify)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
