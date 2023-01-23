package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object RsaVerify extends JsTestBase {
  // rsaVerify
  private val rsaVerify                     = s"rsaVerify($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerifyArgBeforeFunc        = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify              = s"rsaVerify()"
  private val invalidRsaVerifyArgBeforeFunc = s"callerTestData.rsaVerify(callerTestData)"
  private val invalidErrorRsaVerify  = testData.invalidFunctionError("rsaVerify", 4)

  val tests: Tests = Tests {
    test("check: rsaVerify function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: rsaVerify function compiles(argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerifyArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: rsaVerify - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, rsaVerify)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: rsaVerify - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomAddressDataArrayElement, rsaVerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload rsaVerify") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify)
      }
    }

    test("compilation error: Can't find a function overload rsaVerify (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify)
      }
    }
  }
}
