package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Keccak256_16Kb extends JsTestBase {
  private val keccak256_16Kb                     = "keccak256_16Kb(callerTestData)"
  private val keccak256_16KbArgBeforeFunc        = "callerTestData.keccak256_16Kb()"
  private val invalidKeccak256_16Kb              = "keccak256_16Kb()"
  private val invalidKeccak256_16KbArgBeforeFunc = "callerTestData.keccak256_16Kb(callerTestData)"
  private val invalidErrorKeccak256_16Kb         = testData.invalidFunctionError("keccak256_16Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-134. Function keccak256_16Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, keccak256_16Kb),
            (randomByteVectorArrayElement, keccak256_16KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-135. Function keccak256_16Kb should throw an error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, keccak256_16Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, keccak256_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidKeccak256_16Kb, invalidErrorKeccak256_16Kb),
            (randomByteVectorArrayElement, invalidKeccak256_16KbArgBeforeFunc, invalidErrorKeccak256_16Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
