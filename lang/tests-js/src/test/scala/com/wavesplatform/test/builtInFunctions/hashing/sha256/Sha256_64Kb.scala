package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Sha256_64Kb extends JsTestBase {
  private val sha256_64Kb                     = "sha256_64Kb(callerTestData)"
  private val sha256_64KbArgBeforeFunc        = "callerTestData.sha256_64Kb()"
  private val invalidSha256_64Kb              = "sha256_64Kb()"
  private val invalidSha256_64KbArgBeforeFunc = "callerTestData.sha256_64Kb(callerTestData)"
  private val invalidErrorSha256_64Kb         = testData.invalidFunctionError("sha256_64Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-148. Function sha256_64Kb should compile for valid ByteVector") {
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

    test("RIDE-149. Function sha256_64Kb should throw an error for invalid data") {
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
