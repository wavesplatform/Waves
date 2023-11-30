package com.wavesplatform.test.builtInFunctions.blockchain

import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.*
import com.wavesplatform.JsTestBase
import testHelpers.TestDataConstantsAndMethods.thisVariable
import utest.{Tests, test}

object AddressFromRecipient extends JsTestBase {
  private val addressFromRecipient              = "addressFromRecipient(addressOrAlias)"
  private val addressFromRecipientArgBeforeFunc = "addressOrAlias.addressFromRecipient()"
  private val invalidFunc                       = "addressFromRecipient()"

  val tests: Tests = Tests {
    test("RIDE-24. Compile addressFromRecipient function for address, alias, and 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (addressOrAlias, function, address) <- Seq(
            (randomAddressDataArrayElement, addressFromRecipient, randomAddressDataArrayElement),
            (randomAliasDataArrayElement, addressFromRecipient, randomAddressDataArrayElement),
            (thisVariable, addressFromRecipient, randomAddressDataArrayElement),
            (randomAddressDataArrayElement, addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement),
            (randomAliasDataArrayElement, addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement),
            (thisVariable, addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement)
          )
        ) {
          val script = precondition.codeForAddressFromRecipient(addressOrAlias, function, address)
          assertCompileSuccessExpression(script, version)
        }
      }
    }

    test("RIDE-25. Invalid data for functions addressFromRecipient") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (addressOrAlias, function, address) <- Seq(
            (randomInt.toString, addressFromRecipient, randomAddressDataArrayElement),
            (randomInt.toString, addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement)
          )
        ) {
          val script = precondition.codeForAddressFromRecipient(addressOrAlias, function, address)
          assertCompileErrorExpression(script, version, testData.nonMatchingTypes("Address|Alias"))
        }
      }
    }

    test("RIDE-26. Function 'addressFromRecipient' requires 1 arguments") {
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
