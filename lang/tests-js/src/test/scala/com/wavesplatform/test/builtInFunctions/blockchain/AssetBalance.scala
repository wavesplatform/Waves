package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultIntegerEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result, thisVariable}
import utest.{Tests, test}

object AssetBalance extends JsTestBase {
  private val address: String = randomAddressDataArrayElement
  private val alias: String = randomAliasDataArrayElement
  private val byteVector: String = randomByteVectorArrayElement

  private val assetBalance = s"assetBalance(callerTestData, $byteVector)"
  private val assetBalanceArgBeforeFunc = s"callerTestData.assetBalance($byteVector)"
  private val invalidAssetBalanceFunc = "assetBalance()"

  val tests: Tests = Tests {
    test("RIDE-27. Compile assetBalance function for address, alias, and 'this'") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (addressOrAlias, function) <- Seq(
            (address, assetBalance),
            (alias, assetBalance),
            (thisVariable, assetBalance),
            (address, assetBalanceArgBeforeFunc),
            (alias, assetBalanceArgBeforeFunc),
            (thisVariable, assetBalanceArgBeforeFunc),
          )
        ) {
          val script = precondition.codeWithoutMatcher(addressOrAlias, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-28. Invalid data must be validated") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (addressOrAlias, function, error) <- Seq(
            (address, invalidAssetBalanceFunc, invalidFunctionError("assetBalance", 2)),
            (alias, invalidAssetBalanceFunc, invalidFunctionError("assetBalance", 2)),
            (thisVariable, invalidAssetBalanceFunc, invalidFunctionError("assetBalance", 2)),
            (randomUnionArrayElement, assetBalance, nonMatchingTypes("Address|Alias")),
            (randomByteVectorArrayElement, assetBalanceArgBeforeFunc, nonMatchingTypes("Address|Alias")),
          )
        ) {
          val script = precondition.codeWithoutMatcher(addressOrAlias, function, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
