package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Sha256_16Kb extends JsTestBase {
  private val sha256_16Kb                     = "sha256_16Kb(callerTestData)"
  private val sha256_16KbArgBeforeFunc        = "callerTestData.sha256_16Kb()"
  private val invalidSha256_16Kb              = "sha256_16Kb()"
  private val invalidSha256_16KbArgBeforeFunc = "callerTestData.sha256_16Kb(callerTestData)"
  private val invalidErrorSha256_16Kb         = testData.invalidFunctionError("sha256_16Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-144. Function sha256_16Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sha256_16Kb),
            (randomByteVectorArrayElement, sha256_16KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-145. Function sha256_16Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sha256_16Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sha256_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSha256_16Kb, invalidErrorSha256_16Kb),
            (randomByteVectorArrayElement, invalidSha256_16KbArgBeforeFunc, invalidErrorSha256_16Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
