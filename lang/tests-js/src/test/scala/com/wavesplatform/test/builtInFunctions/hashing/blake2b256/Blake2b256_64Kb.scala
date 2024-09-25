package com.wavesplatform.test.builtInFunctions.hashing.blake2b256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersionsWithoutV3, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Blake2b256_64Kb extends JsTestBase {
  private val blake2b256_64Kb                     = "blake2b256_64Kb(callerTestData)"
  private val blake2b256_64KbArgBeforeFunc        = "callerTestData.blake2b256_64Kb()"
  private val invalidBlake2b256_64Kb              = "blake2b256_64Kb()"
  private val invalidBlake2b256_64KbArgBeforeFunc = "callerTestData.blake2b256_64Kb(callerTestData)"
  private val invalidErrorBlake2b256_64Kb         = testData.invalidFunctionError("blake2b256_64Kb", 1)

  val tests: Tests = Tests {
    test("RIDE-128. Function blake2b256_64Kb should compile for valid ByteVector") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, blake2b256_64Kb),
            (randomByteVectorArrayElement, blake2b256_64KbArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-128. Function blake2b256_64Kb должна выдавать ошибку при невалидных данных") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, blake2b256_64Kb, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, blake2b256_64KbArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidBlake2b256_64Kb, invalidErrorBlake2b256_64Kb),
            (randomByteVectorArrayElement, invalidBlake2b256_64KbArgBeforeFunc, invalidErrorBlake2b256_64Kb)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
