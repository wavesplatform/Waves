package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object SigVerify_64Kb extends JsTestBase {
  // sigVerify_64Kb
  private val sigVerify_64Kb              = s"sigVerify_64Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_64KbArgBeforeFunc = s"callerTestData.sigVerify_64Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_64Kb       = "sigVerify_64Kb()"
  private val invalidSigVerify_64KbArgBeforeFunc = "callerTestData.sigVerify_64Kb(callerTestData)"
  private val invalidErrorSigVerify_64Kb         = testData.invalidFunctionError("sigVerify_64Kb", 3)

  val tests: Tests = Tests {
    test.apply("check: sigVerify_64Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_64Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: sigVerify_64Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_64KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: sigVerify_64Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, sigVerify_64Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: sigVerify_64Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, sigVerify_64KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload sigVerify_64Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_64Kb)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_64Kb)
      }
    }

    test.apply("compilation error: Can't find a function overload sigVerify_64Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSigVerify_64KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorSigVerify_64Kb)
      }
    }

    test.apply("compilation error: Can't find a function sigVerify_64Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_64Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
