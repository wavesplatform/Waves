package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object SigVerify extends JsTestBase {
  // sigVerify
  private val sigVerify                     = s"sigVerify(callerTestData, callerTestData, callerTestData)"
  private val sigVerifyArgBeforeFunc        = s"callerTestData.sigVerify(callerTestData, callerTestData)"
  private val invalidSigVerify              = s"sigVerify()"
  private val invalidSigVerifyArgBeforeFunc = s"callerTestData.sigVerify(callerTestData)"
  private val invalidErrorSigVerify  = testData.invalidFunctionError("sigVerify", 3)

  val tests: Tests = Tests {
    test("check: sigVerify function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: sigVerify function compiles(argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerifyArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: sigVerify - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, sigVerify)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: sigVerify - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomAddressDataArrayElement, sigVerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload sigVerify") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify)
      }
    }

    test("compilation error: Can't find a function overload sigVerify (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify)
      }
    }
  }

}
