package com.wavesplatform.test.builtInFunctions.verification.rsaVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object RsaVerify_64Kb extends JsTestBase {
  // rsaVerify_64Kb
  private val rsaVerify_64Kb              = s"rsaVerify_64Kb($randomDigestAlgorithmTypeArrayElement, callerTestData, callerTestData, callerTestData)"
  private val rsaVerify_64KbArgBeforeFunc = s"$randomDigestAlgorithmTypeArrayElement.rsaVerify_64Kb(callerTestData, callerTestData, callerTestData)"
  private val invalidRsaVerify_64Kb       = "rsaVerify_64Kb()"
  private val invalidRsaVerify_64KbArgBeforeFunc = "callerTestData.rsaVerify_64Kb(callerTestData)"
  private val invalidErrorRsaVerify_64Kb         = testData.invalidFunctionError("rsaVerify_64Kb", 4)

  val tests: Tests = Tests {
    test.apply("check: rsaVerify_64Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_64Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: rsaVerify_64Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_64KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: rsaVerify_64Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, rsaVerify_64Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: rsaVerify_64Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, rsaVerify_64KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload rsaVerify_64Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_64Kb)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_64Kb)
      }
    }

    test.apply("compilation error: Can't find a function overload rsaVerify_64Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidRsaVerify_64KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorRsaVerify_64Kb)
      }
    }

    test.apply("compilation error: Can't find a function rsaVerify_64Kb") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, rsaVerify_64Kb)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
