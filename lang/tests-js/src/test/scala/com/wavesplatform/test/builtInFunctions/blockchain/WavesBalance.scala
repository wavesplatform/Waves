package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.thisVariable
import utest.{Tests, test}

object WavesBalance extends JsTestBase {
  private val wavesBalance              = "wavesBalance(callerTestData)"
  private val wavesBalanceArgBeforeFunc = "callerTestData.wavesBalance()"
  private val invalidWavesBalance       = "wavesBalance()"
  private val invalidWavesBalanceArg    = s"$randomInt.wavesBalance()"

  val tests: Tests = Tests {
    test.apply("check: function wavesBalance for version V4 and more compiles for address") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomAddressDataArrayElement,
          wavesBalance
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function wavesBalance for version V4 and more (argument before function) compiles for address") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomAddressDataArrayElement,
          wavesBalanceArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function wavesBalance for version V4 and more compiles for 'this'") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          thisVariable,
          wavesBalance
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function wavesBalance for version V4 and more (argument before function) compiles for 'this'") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          thisVariable,
          wavesBalanceArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function wavesBalance for version V4 and more compiles for alias") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          wavesBalance
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function wavesBalance for version V4 and more (argument before function) compiles for alias") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          wavesBalanceArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: wavesBalance for version V4 and more Non-matching type") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          wavesBalance
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }

    test.apply("compilation error: wavesBalance for version V4 and more Non-matching type (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          invalidWavesBalanceArg
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }

    test.apply("compilation error: Function 'wavesBalance for version V4 and more' requires 1 arguments") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        val script = precondition.onlyMatcherContract(
          randomAddressDataArrayElement,
          invalidWavesBalance
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("wavesBalance", 1))
      }
    }

    test.apply("check: function wavesBalance for V3 compiles for address") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomAddressDataArrayElement,
        wavesBalance
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function wavesBalance for V3 (argument before function) compiles for address") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomAddressDataArrayElement,
        wavesBalanceArgBeforeFunc
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function wavesBalance for V3 compiles for 'this'") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        thisVariable,
        wavesBalance
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function wavesBalance for V3 (argument before function) compiles for 'this'") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        thisVariable,
        wavesBalanceArgBeforeFunc
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function wavesBalance for V3 compiles for alias") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomAliasDataArrayElement,
        wavesBalance
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: function wavesBalance for V3 (argument before function) compiles for alias") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomAliasDataArrayElement,
        wavesBalanceArgBeforeFunc
      )
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("compilation error: wavesBalance for V3 Non-matching type") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomDigestAlgorithmTypeArrayElement,
        wavesBalance
      )
      assertCompileErrorDApp(script, V3, testData.nonMatchingTypes("Address|Alias"))
    }

    test.apply("compilation error: wavesBalance for V3 Non-matching type (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomDigestAlgorithmTypeArrayElement,
        invalidWavesBalanceArg
      )
      assertCompileErrorDApp(script, V3, testData.nonMatchingTypes("Address|Alias"))
    }

    test.apply("compilation error: Function 'wavesBalance for V3' requires 1 arguments") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(
        randomAddressDataArrayElement,
        invalidWavesBalance
      )
      assertCompileErrorDApp(script, V3, testData.invalidFunctionError("wavesBalance", 1))
    }
  }
}
