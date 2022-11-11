package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.test.builtInFunctions.accountDataStorage.GetBinary.testData
import _root_.testData.GeneratorContractsForBuiltInFunctions
import _root_.testData.RandomDataGenerator.*
import com.wavesplatform.JsTestBase
import utest.{Tests, test}

object AddressFromRecipient extends JsTestBase {
  val addressFromRecipient = "addressFromRecipient(addressOrAlias)"
  val addressFromRecipientArgBeforeFunc = "addressOrAlias.addressFromRecipient()"
  val invalidFunc = "addressFromRecipient()"

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
        assertCompileErrorExpression(script, version, testData.NON_MATCHING_TYPES)
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
        assertCompileErrorExpression(script, version, testData.NON_MATCHING_TYPES)
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
        assertCompileErrorExpression(script, version, testData.invalidFunctionError("addressFromRecipient"))
      }
    }


  }
}
