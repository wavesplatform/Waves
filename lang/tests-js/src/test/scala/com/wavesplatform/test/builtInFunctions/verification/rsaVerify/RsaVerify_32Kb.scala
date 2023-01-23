package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object RsaVerify_32Kb extends JsTestBase {
  // rsaVerify_32Kb
  private val rsaVerify_32Kb              = s"rsaVerify_32Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_32KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_32Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_32Kb       = "rsaVerify_32Kb()"
  private val invalidRsaVerify_32KbArgBeforeFunc = "callerTestData.rsaVerify_32Kb(callerTestData)"
  private val invalidErrorRsaVerify_32Kb         = testData.invalidFunctionError("rsaVerify_32Kb", 4)

  val tests: Tests = Tests {
    test("check: rsaVerify_32Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_32Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: rsaVerify_32Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_32KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: rsaVerify_32Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, rsaVerify_32Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: rsaVerify_32Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, rsaVerify_32KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload rsaVerify_32Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_32Kb)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_32Kb)
      }
    }

    test("compilation error: Can't find a function overload rsaVerify_32Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_32KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_32Kb)
      }
    }

    test("compilation error: Can't find a function rsaVerify_32Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_32Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
