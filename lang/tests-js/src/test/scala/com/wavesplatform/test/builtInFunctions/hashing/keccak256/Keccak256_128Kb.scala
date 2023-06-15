package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Keccak256_128Kb extends JsTestBase {
  private val keccak256_128Kb                     = "keccak256_128Kb(callerTestData)"
  private val keccak256_128KbArgBeforeFunc        = "callerTestData.keccak256_128Kb()"
  private val invalidKeccak256_128Kb              = "keccak256_128Kb()"
  private val invalidKeccak256_128KbArgBeforeFunc = "callerTestData.keccak256_128Kb(callerTestData)"
  private val invalidErrorKeccak256_128Kb         = testData.invalidFunctionError("keccak256_128Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-140. Function keccak256_128Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, keccak256_128Kb),
            (randomByteVectorArrayElement, keccak256_128KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-141. Function keccak256_128Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, keccak256_128Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, keccak256_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidKeccak256_128Kb, invalidErrorKeccak256_128Kb),
            (randomByteVectorArrayElement, invalidKeccak256_128KbArgBeforeFunc, invalidErrorKeccak256_128Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
