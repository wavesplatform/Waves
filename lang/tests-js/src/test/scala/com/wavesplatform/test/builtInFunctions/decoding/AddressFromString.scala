package com.wavesplatform.test.builtInFunctions.decoding

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomBoolean, randomStringArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object AddressFromString extends JsTestBase {
  private val addressFromString                             = "addressFromString(callerTestData)"
  private val addressFromStringArgBeforeFunc                = "callerTestData.addressFromString()"
  private val addressFromStringValue                        = "addressFromStringValue(callerTestData)"
  private val addressFromStringValueArgBeforeFunc           = "callerTestData.addressFromStringValue()"
  private val invalidAddressFromString                      = "addressFromString()"
  private val invalidAddressFromStringValue                 = "addressFromStringValue()"
  private val invalidFunctionErrorForAddressFromString      = testData.invalidFunctionError("addressFromString", 1)
  private val invalidFunctionErrorForAddressFromStringValue = testData.invalidFunctionError("addressFromStringValue", 1)

  val tests: Tests = Tests {
    test.apply("check: function addressFromString compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          addressFromString
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function addressFromString compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          addressFromStringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function addressFromStringValue compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          addressFromStringValue
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function addressFromStringValue compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          addressFromStringValueArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: addressFromString invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          addressFromString
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test.apply("compilation error: addressFromString invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          addressFromStringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test.apply("compilation error: addressFromStringValue invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomBoolean.toString,
          addressFromStringValue
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test.apply("compilation error: addressFromStringValue invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          addressFromStringValueArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test.apply("compilation error: addressFromString invalid function") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidAddressFromString
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorForAddressFromString)
      }
    }

    test.apply("compilation error: addressFromStringValue invalid function") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidAddressFromStringValue
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorForAddressFromStringValue)
      }
    }
  }
}
