package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object AddressFromPublicKey extends JsTestBase {
  private val addressFromPublicKey                     = s"addressFromPublicKey(callerTestData)"
  private val addressFromPublicKeyArgBeforeFunction    = s"callerTestData.addressFromPublicKey()"
  private val invalidAddressFromPublicKey              = s"addressFromPublicKey()"
  private val invalidAddressFromPublicKeyArgBeforeFunc = s"callerTestData.addressFromPublicKey(callerTestData, callerTestData)"

  val tests: Tests = Tests {
    test.apply("check: function addressFromPublicKey compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          addressFromPublicKey
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function addressFromPublicKey compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          addressFromPublicKeyArgBeforeFunction
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomAddressDataArrayElement,
          addressFromPublicKey
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          addressFromPublicKeyArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: invalid function addressFromPublicKey Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidAddressFromPublicKey
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("addressFromPublicKey", 1))
      }
    }

    test.apply("compilation error: invalid function addressFromPublicKey Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidAddressFromPublicKeyArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("addressFromPublicKey", 1))
      }
    }
  }
}
