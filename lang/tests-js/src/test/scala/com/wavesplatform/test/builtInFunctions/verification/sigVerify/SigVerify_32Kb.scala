package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object SigVerify_32Kb extends JsTestBase {
  // sigVerify_32Kb
  private val sigVerify_32Kb              = s"sigVerify_32Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_32KbArgBeforeFunc = s"callerTestData.sigVerify_32Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_32Kb       = "sigVerify_32Kb()"
  private val invalidSigVerify_32KbArgBeforeFunc = "callerTestData.sigVerify_32Kb(callerTestData)"
  private val invalidErrorSigVerify_32Kb         = testData.invalidFunctionError("sigVerify_32Kb", 3)

  val tests: Tests = Tests {
    test.apply("check: sigVerify_32Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_32Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: sigVerify_32Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_32KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: sigVerify_32Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, sigVerify_32Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: sigVerify_32Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, sigVerify_32KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload sigVerify_32Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_32Kb)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_32Kb)
      }
    }

    test.apply("compilation error: Can't find a function overload sigVerify_32Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_32KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_32Kb)
      }
    }

    test.apply("compilation error: Can't find a function sigVerify_32Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_32Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
