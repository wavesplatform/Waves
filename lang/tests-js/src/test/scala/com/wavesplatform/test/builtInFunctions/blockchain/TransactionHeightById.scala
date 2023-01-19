package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement}
import utest.{Tests, test}

object TransactionHeightById extends JsTestBase {
  private val transactionHeightById = "transactionHeightById(callerTestData)"
  private val transactionHeightByIdArgBeforeFunc = "callerTestData.transactionHeightById()"

  private val invalidTransactionHeightById = "transactionHeightById()"
  private val invalidTransactionHeightByIdArg = s"$randomAliasDataArrayElement.transactionHeightById()"


  val tests: Tests = Tests {
    test.apply("check: function transactionHeightById compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transactionHeightById
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function transactionHeightById (argument before function) compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transactionHeightByIdArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: transactionHeightById Non-matching type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          transactionHeightById
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: transactionHeightById Non-matching type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          invalidTransactionHeightByIdArg
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Function 'transactionHeightById' requires 1 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidTransactionHeightById
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("transactionHeightById", 1))
      }
    }
  }
}
