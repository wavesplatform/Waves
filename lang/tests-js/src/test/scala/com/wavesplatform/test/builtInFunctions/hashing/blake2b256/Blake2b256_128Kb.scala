package com.wavesplatform.test.builtInFunctions.hashing.blake2b256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Blake2b256_128Kb extends JsTestBase {
  // blake2b256_128Kb
  private val blake2b256_128Kb                     = "blake2b256_128Kb(callerTestData)"
  private val blake2b256_128KbArgBeforeFunc        = "callerTestData.blake2b256_128Kb()"
  private val invalidBlake2b256_128Kb              = "blake2b256_128Kb()"
  private val invalidBlake2b256_128KbArgBeforeFunc = "callerTestData.blake2b256_128Kb(callerTestData)"
  private val invalidErrorBlake2b256_128Kb  = testData.invalidFunctionError("blake2b256_128Kb", 1)

  val tests: Tests = Tests {
    test("blake2b256_128Kb functions compiles with a ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, blake2b256_128Kb),
            (randomByteVectorArrayElement, blake2b256_128KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("compilation errors blake2b256_128Kb") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, blake2b256_128Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, blake2b256_128KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidBlake2b256_128Kb, invalidErrorBlake2b256_128Kb),
            (randomByteVectorArrayElement, invalidBlake2b256_128KbArgBeforeFunc, invalidErrorBlake2b256_128Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }

}
