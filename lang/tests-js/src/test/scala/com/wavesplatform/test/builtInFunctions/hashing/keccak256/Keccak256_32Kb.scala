package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Keccak256_32Kb extends JsTestBase {
  private val keccak256_32Kb                     = "keccak256_32Kb(callerTestData)"
  private val keccak256_32KbArgBeforeFunc        = "callerTestData.keccak256_32Kb()"
  private val invalidKeccak256_32Kb              = "keccak256_32Kb()"
  private val invalidKeccak256_32KbArgBeforeFunc = "callerTestData.keccak256_32Kb(callerTestData)"
  private val invalidErrorKeccak256_32Kb         = testData.invalidFunctionError("keccak256_32Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-136. Function keccak256_32Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, keccak256_32Kb),
            (randomByteVectorArrayElement, keccak256_32KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-137. Function keccak256_32Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, keccak256_32Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, keccak256_32KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidKeccak256_32Kb, invalidErrorKeccak256_32Kb),
            (randomByteVectorArrayElement, invalidKeccak256_32KbArgBeforeFunc, invalidErrorKeccak256_32Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
