package com.wavesplatform.test.builtInFunctions.verification.sigVerify

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object SigVerify_64Kb extends JsTestBase {
  private val sigVerify_64Kb                     = s"sigVerify_64Kb(callerTestData, callerTestData, callerTestData)"
  private val sigVerify_64KbArgBeforeFunc        = s"callerTestData.sigVerify_64Kb(callerTestData, callerTestData)"
  private val invalidSigVerify_64Kb              = "sigVerify_64Kb()"
  private val invalidSigVerify_64KbArgBeforeFunc = "callerTestData.sigVerify_64Kb(callerTestData)"
  private val invalidErrorSigVerify_64Kb         = testData.invalidFunctionError("sigVerify_64Kb", 3)

  val tests: Tests = Tests {
    test("RIDE-259. sigVerify_64Kb function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sigVerify_64Kb),
            (randomByteVectorArrayElement, sigVerify_64KbArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-260. sigVerify_64Kb function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sigVerify_64Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sigVerify_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSigVerify_64Kb, invalidErrorSigVerify_64Kb),
            (randomByteVectorArrayElement, invalidSigVerify_64KbArgBeforeFunc, invalidErrorSigVerify_64Kb)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-261. Can't find a function sigVerify_64Kb for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, sigVerify_64Kb)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
