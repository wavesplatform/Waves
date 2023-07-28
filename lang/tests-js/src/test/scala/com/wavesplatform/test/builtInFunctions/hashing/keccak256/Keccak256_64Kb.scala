package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Keccak256_64Kb extends JsTestBase {
  private val keccak256_64Kb                     = "keccak256_64Kb(callerTestData)"
  private val keccak256_64KbArgBeforeFunc        = "callerTestData.keccak256_64Kb()"
  private val invalidKeccak256_64Kb              = "keccak256_64Kb()"
  private val invalidKeccak256_64KbArgBeforeFunc = "callerTestData.keccak256_64Kb(callerTestData)"
  private val invalidErrorKeccak256_64Kb         = testData.invalidFunctionError("keccak256_64Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-138. Function keccak256_64Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, keccak256_64Kb),
            (randomByteVectorArrayElement, keccak256_64KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-139. Function keccak256_64Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, keccak256_64Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, keccak256_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidKeccak256_64Kb, invalidErrorKeccak256_64Kb),
            (randomByteVectorArrayElement, invalidKeccak256_64KbArgBeforeFunc, invalidErrorKeccak256_64Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
