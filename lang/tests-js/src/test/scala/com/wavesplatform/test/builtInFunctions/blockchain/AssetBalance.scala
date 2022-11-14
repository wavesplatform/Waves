package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import testData.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object AssetBalance extends JsTestBase {
  private val address: String = randomAddressDataArrayElement
  private val alias: String = randomAliasDataArrayElement
  private val byteVector: String = randomByteVectorArrayElement

  private val assetBalance = s"assetBalance(callerTestData, $byteVector)"
  private val assetBalanceArgBeforeFunc = s"callerTestData.assetBalance($byteVector)"
  private val invalidAssetBalanceFunc = "assetBalance()"
  private val testData = new TestDataConstantsAndMethods

  val tests: Tests = Tests {
    test.apply("check: function AssetBalance compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          address,
          assetBalance,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function AssetBalance compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          alias,
          assetBalance,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function AssetBalance (argument before function) compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          address,
          assetBalanceArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function AssetBalance (argument before function) compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          alias,
          assetBalanceArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Function 'assetBalance' requires 1 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          address,
          invalidAssetBalanceFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("assetBalance", 2))
      }
    }

    test.apply("compilation error: Function 'assetBalance' requires 2 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          address,
          invalidAssetBalanceFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("assetBalance", 2))
      }
    }

    test.apply("compilation error: assetBalance Non-matching types: expected: Address|Alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeWithoutMatcher(
          randomUnionArrayElement,
          assetBalanceArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }
  }

}
