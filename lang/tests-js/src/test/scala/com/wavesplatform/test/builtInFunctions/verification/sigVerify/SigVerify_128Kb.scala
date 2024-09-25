package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object SigVerify_128Kb extends JsTestBase {
  private val sigVerify_128Kb                     = s"sigVerify_128Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_128KbArgBeforeFunc        = s"callerTestData.sigVerify_128Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_128Kb              = "sigVerify_128Kb()"
  private val invalidSigVerify_128KbArgBeforeFunc = "callerTestData.sigVerify_128Kb(callerTestData)"
  private val invalidErrorSigVerify_128Kb         = testData.invalidFunctionError("sigVerify_128Kb", 3)

  val tests: Tests = Tests {
    test("RIDE-262. sigVerify_128Kb function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sigVerify_128Kb),
            (randomByteVectorArrayElement, sigVerify_128KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-263. sigVerify_128Kb function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sigVerify_128Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sigVerify_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSigVerify_128Kb, invalidErrorSigVerify_128Kb),
            (randomByteVectorArrayElement, invalidSigVerify_128KbArgBeforeFunc, invalidErrorSigVerify_128Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-264. Can't find a function sigVerify_128Kb for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_128Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
