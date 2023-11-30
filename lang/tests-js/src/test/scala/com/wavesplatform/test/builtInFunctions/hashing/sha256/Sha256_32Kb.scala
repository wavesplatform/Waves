package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Sha256_32Kb extends JsTestBase {
  private val sha256_32Kb                     = "sha256_32Kb(callerTestData)"
  private val sha256_32KbArgBeforeFunc        = "callerTestData.sha256_32Kb()"
  private val invalidSha256_32Kb              = "sha256_32Kb()"
  private val invalidSha256_32KbArgBeforeFunc = "callerTestData.sha256_32Kb(callerTestData)"
  private val invalidErrorSha256_32Kb         = testData.invalidFunctionError("sha256_32Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-146. Function sha256_32Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sha256_32Kb),
            (randomByteVectorArrayElement, sha256_32KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-147. Function sha256_32Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sha256_32Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sha256_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSha256_32Kb, invalidErrorSha256_32Kb),
            (randomByteVectorArrayElement, invalidSha256_32KbArgBeforeFunc, invalidErrorSha256_32Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
