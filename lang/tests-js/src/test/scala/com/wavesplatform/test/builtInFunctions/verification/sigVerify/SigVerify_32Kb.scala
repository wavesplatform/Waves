package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object SigVerify_32Kb extends JsTestBase {
  private val sigVerify_32Kb                     = s"sigVerify_32Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_32KbArgBeforeFunc        = s"callerTestData.sigVerify_32Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_32Kb              = "sigVerify_32Kb()"
  private val invalidSigVerify_32KbArgBeforeFunc = "callerTestData.sigVerify_32Kb(callerTestData)"
  private val invalidErrorSigVerify_32Kb         = testData.invalidFunctionError("sigVerify_32Kb", 3)

  val tests: Tests = Tests {
    test("RIDE-256. sigVerify_32Kb function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sigVerify_32Kb),
            (randomByteVectorArrayElement, sigVerify_32KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-257. sigVerify_32Kb function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sigVerify_32Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sigVerify_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSigVerify_32Kb, invalidErrorSigVerify_32Kb),
            (randomByteVectorArrayElement, invalidSigVerify_32KbArgBeforeFunc, invalidErrorSigVerify_32Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-258. Can't find a function sigVerify_32Kb for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_32Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
