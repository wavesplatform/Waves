package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, thisVariable}
import utest.{Tests, test}

object WavesBalance extends JsTestBase {
  private val wavesBalance              = "wavesBalance(callerTestData)"
  private val wavesBalanceArgBeforeFunc = "callerTestData.wavesBalance()"
  private val invalidWavesBalance       = "wavesBalance()"
  private val invalidWavesBalanceArg    = s"callerTestData.wavesBalance(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-46. wavesBalance function for version V4 and more should compile for address, alias, and 'this'") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, wavesBalance),
            (randomAliasDataArrayElement, wavesBalance),
            (thisVariable, wavesBalance),
            (randomAddressDataArrayElement, wavesBalanceArgBeforeFunc),
            (randomAliasDataArrayElement, wavesBalanceArgBeforeFunc),
            (thisVariable, wavesBalanceArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-47. Negative cases for wavesBalance function for version V4 and more") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BalanceDetails", version)
        for (
          (data, function, error) <- Seq(
            (randomDigestAlgorithmTypeArrayElement, wavesBalance, nonMatchingTypes("Address|Alias")),
            (randomStringArrayElement, wavesBalanceArgBeforeFunc, nonMatchingTypes("Address|Alias")),
            (randomAddressDataArrayElement, invalidWavesBalanceArg, invalidFunctionError("wavesBalance", 1)),
            (randomAliasDataArrayElement, invalidWavesBalance, invalidFunctionError("wavesBalance", 1))
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-48. Functions wavesBalance for V3 compiles for address, alias and 'this'") {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, wavesBalance),
            (randomAliasDataArrayElement, wavesBalance),
            (thisVariable, wavesBalance),
            (randomAddressDataArrayElement, wavesBalanceArgBeforeFunc),
            (randomAliasDataArrayElement, wavesBalanceArgBeforeFunc),
            (thisVariable, wavesBalanceArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, V3)
      }
    }

    test("RIDE-49. compilation error: wavesBalance for V3 Non-matching type") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      for (
        (data, function, error) <- Seq(
          (randomDigestAlgorithmTypeArrayElement, wavesBalance, nonMatchingTypes("Address|Alias")),
          (randomStringArrayElement, wavesBalanceArgBeforeFunc, nonMatchingTypes("Address|Alias")),
          (randomAddressDataArrayElement, invalidWavesBalanceArg, invalidFunctionError("wavesBalance", 1)),
          (randomAliasDataArrayElement, invalidWavesBalance, invalidFunctionError("wavesBalance", 1))
        )
      ) {
        val script = precondition.onlyMatcherContract(data, function)
        assertCompileErrorDApp(script, V3, error)
      }
    }
  }
}
