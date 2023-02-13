package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Sha256_64Kb extends JsTestBase {
  // sha256_64Kb
  private val sha256_64Kb                     = "sha256_64Kb(callerTestData)"
  private val sha256_64KbArgBeforeFunc        = "callerTestData.sha256_64Kb()"
  private val invalidSha256_64Kb              = "sha256_64Kb()"
  private val invalidSha256_64KbArgBeforeFunc = "callerTestData.sha256_64Kb(callerTestData)"
  private val invalidErrorSha256_64Kb         = testData.invalidFunctionError("sha256_64Kb", 1)

  val tests: Tests = Tests {
    test("sha256_64Kb functions compiles with a ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sha256_64Kb),
            (randomByteVectorArrayElement, sha256_64KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("compilation errors sha256_64Kb") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sha256_64Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sha256_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSha256_64Kb, invalidErrorSha256_64Kb),
            (randomByteVectorArrayElement, invalidSha256_64KbArgBeforeFunc, invalidErrorSha256_64Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
