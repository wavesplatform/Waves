package com.wavesplatform.test.builtInFunctions.hashing.blake2b256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Blake2b256_16Kb extends JsTestBase {
  // blake2b256_16Kb
  private val blake2b256_16Kb                     = "blake2b256_16Kb(callerTestData)"
  private val blake2b256_16KbArgBeforeFunc        = "callerTestData.blake2b256_16Kb()"
  private val invalidBlake2b256_16Kb              = "blake2b256_16Kb()"
  private val invalidBlake2b256_16KbArgBeforeFunc = "callerTestData.blake2b256_16Kb(callerTestData)"
  private val invalidErrorBlake2b256_16Kb         = testData.invalidFunctionError("blake2b256_16Kb", 1)

  val tests: Tests = Tests {
    test("blake2b256_16Kb functions compiles with a ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, blake2b256_16Kb),
            (randomByteVectorArrayElement, blake2b256_16KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("compilation errors blake2b256_16Kb") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, blake2b256_16Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, blake2b256_16KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidBlake2b256_16Kb, invalidErrorBlake2b256_16Kb),
            (randomByteVectorArrayElement, invalidBlake2b256_16KbArgBeforeFunc, invalidErrorBlake2b256_16Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
