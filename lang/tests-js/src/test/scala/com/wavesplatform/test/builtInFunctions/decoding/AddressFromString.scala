package com.wavesplatform.test.builtInFunctions.decoding

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomBoolean, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object AddressFromString extends JsTestBase {
  private val addressFromString                             = "addressFromString(callerTestData)"
  private val addressFromStringArgBeforeFunc                = "callerTestData.addressFromString()"
  private val addressFromStringValue                        = "addressFromStringValue(callerTestData)"
  private val addressFromStringValueArgBeforeFunc           = "callerTestData.addressFromStringValue()"
  private val invalidAddressFromString                      = "addressFromString()"
  private val invalidAddressFromStringValue                 = "addressFromStringValue()"
  private val invalidFunctionErrorForAddressFromString      = invalidFunctionError("addressFromString", 1)
  private val invalidFunctionErrorForAddressFromStringValue = invalidFunctionError("addressFromStringValue", 1)

  val tests: Tests = Tests {
    test("Functions addressFromString compiles") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, addressFromString),
            (randomStringArrayElement, addressFromStringArgBeforeFunc),
            (randomStringArrayElement, addressFromStringValue),
            (randomStringArrayElement, addressFromStringValueArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation error: addressFromString invalid data or function") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        for (
          (data, function, error) <- Seq(
            (randomAliasDataArrayElement, addressFromString, nonMatchingTypes("String")),
            (randomAliasDataArrayElement, addressFromStringArgBeforeFunc, nonMatchingTypes("String")),
            (randomBoolean.toString, addressFromStringValue, nonMatchingTypes("String")),
            (randomUnionArrayElement, addressFromStringValueArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, invalidAddressFromString, invalidFunctionErrorForAddressFromString),
            (randomStringArrayElement, invalidAddressFromStringValue, invalidFunctionErrorForAddressFromStringValue)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
