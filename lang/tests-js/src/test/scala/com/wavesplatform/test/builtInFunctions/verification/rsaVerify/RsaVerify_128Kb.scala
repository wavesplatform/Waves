package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object RsaVerify_128Kb extends JsTestBase {
  // rsaVerify_128Kb
  private val rsaVerify_128Kb              = s"rsaVerify_128Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_128KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_128Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_128Kb       = "rsaVerify_128Kb()"
  private val invalidRsaVerify_128KbArgBeforeFunc = "callerTestData.rsaVerify_128Kb(callerTestData)"
  private val invalidErrorRsaVerify_128Kb         = testData.invalidFunctionError("rsaVerify_128Kb", 4)

  val tests: Tests = Tests {
    test("check: rsaVerify_128Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_128Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: rsaVerify_128Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_128KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: rsaVerify_128Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, rsaVerify_128Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: rsaVerify_128Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, rsaVerify_128KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload rsaVerify_128Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_128Kb)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_128Kb)
      }
    }

    test("compilation error: Can't find a function overload rsaVerify_128Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_128KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_128Kb)
      }
    }

    test("compilation error: Can't find a function rsaVerify_128Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_128Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
