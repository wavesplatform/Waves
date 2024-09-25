package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersions, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Keccak256 extends JsTestBase {
  private val keccak256                     = "keccak256(callerTestData)"
  private val keccak256ArgBeforeFunc        = "callerTestData.keccak256()"
  private val invalidKeccak256              = "keccak256()"
  private val invalidKeccak256ArgBeforeFunc = "callerTestData.keccak256(callerTestData)"
  private val invalidErrorKeccak256         = testData.invalidFunctionError("keccak256", 1)

  val tests: Tests = Tests {
    test("RIDE-132. Function Keccak256 should compile for valid ByteVector") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, keccak256),
            (randomByteVectorArrayElement, keccak256ArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-133. Function blake2b256 should throw an error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, keccak256, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, keccak256ArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidKeccak256, invalidErrorKeccak256),
            (randomByteVectorArrayElement, invalidKeccak256ArgBeforeFunc, invalidErrorKeccak256)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
