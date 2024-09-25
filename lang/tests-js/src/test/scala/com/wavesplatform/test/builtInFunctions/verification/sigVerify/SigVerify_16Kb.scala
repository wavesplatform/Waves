package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object SigVerify_16Kb extends JsTestBase {
  private val sigVerify_16Kb                     = s"sigVerify_16Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_16KbArgBeforeFunc        = s"callerTestData.sigVerify_16Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_16Kb              = "sigVerify_16Kb()"
  private val invalidSigVerify_16KbArgBeforeFunc = "callerTestData.sigVerify_16Kb(callerTestData)"
  private val invalidErrorSigVerify_16Kb         = testData.invalidFunctionError("sigVerify_16Kb", 3)

  val tests: Tests = Tests {
    test("RIDE-253. sigVerify_16Kb function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sigVerify_16Kb),
            (randomByteVectorArrayElement, sigVerify_16KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-254. sigVerify_16Kb function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sigVerify_16Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sigVerify_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSigVerify_16Kb, invalidErrorSigVerify_16Kb),
            (randomByteVectorArrayElement, invalidSigVerify_16KbArgBeforeFunc, invalidErrorSigVerify_16Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-255. Can't find a function sigVerify_16Kb for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_16Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
