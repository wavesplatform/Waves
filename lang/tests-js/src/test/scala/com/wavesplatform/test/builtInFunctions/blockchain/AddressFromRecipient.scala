package com.wavesplatform.test.builtInFunctions.blockchain

import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.*
import com.wavesplatform.JsTestBase
import testHelpers.TestDataConstantsAndMethods.thisVariable
import utest.{Tests, test}

object AddressFromRecipient extends JsTestBase {
  private val addressFromRecipient = "addressFromRecipient(addressOrAlias)"
  private val addressFromRecipientArgBeforeFunc = "addressOrAlias.addressFromRecipient()"
  private val invalidFunc = "addressFromRecipient()"

  val tests: Tests = Tests {
    test.apply("check: function addressFromRecipient compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomAddressDataArrayElement,
          addressFromRecipient,
          randomAddressDataArrayElement
        )
        assertCompileSuccessExpression(script, version)
      }
    }

    test.apply("check: function addressFromRecipient compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          thisVariable,
          addressFromRecipient,
          randomAddressDataArrayElement
        )
        assertCompileSuccessExpression(script, version)
      }
    }

    test.apply("check: function addressFromRecipient compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomAliasDataArrayElement,
          addressFromRecipient,
          randomAddressDataArrayElement
        )
        assertCompileSuccessExpression(script, version)
      }
    }

    test.apply("check: function addressFromRecipient (argument before function) compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomAddressDataArrayElement,
          addressFromRecipientArgBeforeFunc,
          randomAddressDataArrayElement
        )
        assertCompileSuccessExpression(script, version)
      }
    }

    test.apply("check: function addressFromRecipient (argument before function) compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          thisVariable,
          addressFromRecipientArgBeforeFunc,
          randomAddressDataArrayElement
        )
        assertCompileSuccessExpression(script, version)
      }
    }

    test.apply("check: function addressFromRecipient (argument before function) compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomAliasDataArrayElement,
          addressFromRecipientArgBeforeFunc,
          randomAddressDataArrayElement
        )
        assertCompileSuccessExpression(script, version)
      }
    }

    test.apply("compilation error: addressFromRecipient Non-matching types: expected: Address|Alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomInt.toString,
          addressFromRecipient,
          randomAddressDataArrayElement
        )
        assertCompileErrorExpression(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }

    test.apply("compilation error: addressFromRecipient (argument before func) Non-matching types: expected: Address|Alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomInt.toString,
          addressFromRecipientArgBeforeFunc,
          randomAddressDataArrayElement
        )
        assertCompileErrorExpression(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }

    test.apply("compilation error: Function 'addressFromRecipient' requires 1 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomAddressDataArrayElement,
          invalidFunc,
          randomAddressDataArrayElement
        )
        assertCompileErrorExpression(script, version, testData.invalidFunctionError("addressFromRecipient", 1))
      }
    }


  }
}
