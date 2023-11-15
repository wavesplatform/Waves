package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Sha256_128Kb extends JsTestBase {
  private val sha256_128Kb                     = "sha256_128Kb(callerTestData)"
  private val sha256_128KbArgBeforeFunc        = "callerTestData.sha256_128Kb()"
  private val invalidSha256_128Kb              = "sha256_128Kb()"
  private val invalidSha256_128KbArgBeforeFunc = "callerTestData.sha256_128Kb(callerTestData)"
  private val invalidErrorSha256_128Kb         = testData.invalidFunctionError("sha256_128Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-150. Function sha256_128Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sha256_128Kb),
            (randomByteVectorArrayElement, sha256_128KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-151. Function sha256_128Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sha256_128Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sha256_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSha256_128Kb, invalidErrorSha256_128Kb),
            (randomByteVectorArrayElement, invalidSha256_128KbArgBeforeFunc, invalidErrorSha256_128Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
