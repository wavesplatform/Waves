package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.test.builtInFunctions.accountDataStorage.GetBinary.{assertCompileSuccessDApp, getBinary, testData}
import _root_.testData.GeneratorContractsForBuiltInFunctions
import _root_.testData.RandomDataGenerator.randomAddressDataArrayElement
import utest.{Tests, test}

object AddressFromRecipient {
  val addressFromRecipient = "addressFromRecipient(addressOrAlias)"
  val addressFromRecipientArgBeforeFunc = "addressOrAlias.addressFromRecipient()"
  val invalidFunc = "addressFromRecipient(addressOrAlias)"

  val tests: Tests = Tests {
    test.apply("check: function getBinary compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForAddressFromRecipient(
          randomAddressDataArrayElement,
          addressFromRecipient,
          randomAddressDataArrayElement
        )


      }
    }
  }
}
