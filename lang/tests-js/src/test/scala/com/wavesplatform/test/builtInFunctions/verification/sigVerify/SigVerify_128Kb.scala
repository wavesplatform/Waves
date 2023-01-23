package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object SigVerify_128Kb extends JsTestBase {
  // sigVerify_128Kb
  private val sigVerify_128Kb              = s"sigVerify_128Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_128KbArgBeforeFunc = s"callerTestData.sigVerify_128Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_128Kb       = "sigVerify_128Kb()"
  private val invalidSigVerify_128KbArgBeforeFunc = "callerTestData.sigVerify_128Kb(callerTestData)"
  private val invalidErrorSigVerify_128Kb         = testData.invalidFunctionError("sigVerify_128Kb", 3)

  val tests: Tests = Tests {
    test("check: sigVerify_128Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_128Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: sigVerify_128Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_128KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: sigVerify_128Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, sigVerify_128Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: sigVerify_128Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, sigVerify_128KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload sigVerify_128Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_128Kb)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_128Kb)
      }
    }

    test("compilation error: Can't find a function overload sigVerify_128Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_128KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_128Kb)
      }
    }

    test("compilation error: Can't find a function sigVerify_128Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_128Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
