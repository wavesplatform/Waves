package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object SigVerify_16Kb extends JsTestBase {
  // sigVerify_16Kb
  private val sigVerify_16Kb              = s"sigVerify_16Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_16KbArgBeforeFunc = s"callerTestData.sigVerify_16Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_16Kb       = "sigVerify_16Kb()"
  private val invalidSigVerify_16KbArgBeforeFunc = "callerTestData.sigVerify_16Kb(callerTestData)"
  private val invalidErrorSigVerify_16Kb         = testData.invalidFunctionError("sigVerify_16Kb", 3)

  val tests: Tests = Tests {
    test("check: sigVerify_16Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_16Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: sigVerify_16Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_16KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: sigVerify_16Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, sigVerify_16Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: sigVerify_16Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, sigVerify_16KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload sigVerify_16Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_16Kb)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_16Kb)
      }
    }

    test("compilation error: Can't find a function overload sigVerify_16Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_16KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_16Kb)
      }
    }

    test("compilation error: Can't find a function sigVerify_16Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_16Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
