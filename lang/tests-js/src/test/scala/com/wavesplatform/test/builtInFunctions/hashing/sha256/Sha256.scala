package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, actualVersions, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object Sha256 extends JsTestBase {
  // sha256
  private val sha256                     = "sha256(callerTestData)"
  private val sha256ArgBeforeFunc        = "callerTestData.sha256()"
  private val invalidSha256              = "sha256()"
  private val invalidSha256ArgBeforeFunc = "callerTestData.sha256(callerTestData)"
  private val invalidErrorSha256         = testData.invalidFunctionError("sha256", 1)

  val tests: Tests = Tests {
    test("blake2b256 functions compiles with a ByteVector") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, sha256),
            (randomByteVectorArrayElement, sha256ArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("compilation errors sha256") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, sha256, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, sha256ArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidSha256, invalidErrorSha256),
            (randomByteVectorArrayElement, invalidSha256ArgBeforeFunc, invalidErrorSha256)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
