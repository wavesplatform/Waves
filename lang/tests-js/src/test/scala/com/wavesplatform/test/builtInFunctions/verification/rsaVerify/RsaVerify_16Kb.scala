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
import utest.{Tests, test}

object RsaVerify_16Kb extends JsTestBase {
  // rsaVerify_16Kb
  private val rsaVerify_16Kb              = s"rsaVerify_16Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_16KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_16Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_16Kb       = "rsaVerify_16Kb()"
  private val invalidRsaVerify_16KbArgBeforeFunc = "callerTestData.rsaVerify_16Kb(callerTestData)"
  private val invalidErrorRsaVerify_16Kb         = testData.invalidFunctionError("rsaVerify_16Kb", 4)

  val tests: Tests = Tests {
    test("check: rsaVerify_16Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_16Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: rsaVerify_16Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_16KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: rsaVerify_16Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, rsaVerify_16Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: rsaVerify_16Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, rsaVerify_16KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload rsaVerify_16Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_16Kb)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_16Kb)
      }
    }

    test("compilation error: Can't find a function overload rsaVerify_16Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_16KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_16Kb)
      }
    }

    test("compilation error: Can't find a function rsaVerify_16Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_16Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
