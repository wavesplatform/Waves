package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object AddressFromPublicKey extends JsTestBase {
  private val addressFromPublicKey                     = s"addressFromPublicKey(callerTestData)"
  private val addressFromPublicKeyArgBeforeFunction    = s"callerTestData.addressFromPublicKey()"
  private val invalidAddressFromPublicKey              = s"addressFromPublicKey()"
  private val invalidAddressFromPublicKeyArgBeforeFunc = s"callerTestData.addressFromPublicKey(callerTestData, callerTestData)"
  private val invalidAddressFromPublicKeyData          = s"addressFromPublicKey(callerTestData, $randomUnionArrayElement)"

  val tests: Tests = Tests {
    test("RIDE-60. AddressFromPublicKey function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, addressFromPublicKey),
            (randomByteVectorArrayElement, addressFromPublicKeyArgBeforeFunction)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-61. AddressFromPublicKey function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomAddressDataArrayElement, addressFromPublicKey, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, addressFromPublicKeyArgBeforeFunction, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidAddressFromPublicKey, invalidFunctionError("addressFromPublicKey", 1)),
            (randomAddressDataArrayElement, invalidAddressFromPublicKeyArgBeforeFunc, invalidFunctionError("addressFromPublicKey", 1)),
            (randomAddressDataArrayElement, invalidAddressFromPublicKeyData, invalidFunctionError("addressFromPublicKey", 1))
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
