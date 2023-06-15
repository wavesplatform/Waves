package com.wavesplatform.test.builtInFunctions.hashing.blake2b256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Blake2b256 extends JsTestBase {
  private val blake2b256                     = "blake2b256(callerTestData)"
  private val blake2b256ArgBeforeFunc        = "callerTestData.blake2b256()"
  private val invalidBlake2b256              = "blake2b256()"
  private val invalidBlake2b256ArgBeforeFunc = "callerTestData.blake2b256(callerTestData)"
  private val invalidErrorBlake2b256         = invalidFunctionError("blake2b256", 1)

  val tests: Tests = Tests {
    test("RIDE-122. Function blake2b256 should compile for valid ByteVector") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, blake2b256),
            (randomByteVectorArrayElement, blake2b256ArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-123. Function blake2b256 should throw an error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, blake2b256, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, blake2b256ArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidBlake2b256, invalidErrorBlake2b256),
            (randomByteVectorArrayElement, invalidBlake2b256ArgBeforeFunc, invalidErrorBlake2b256)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
