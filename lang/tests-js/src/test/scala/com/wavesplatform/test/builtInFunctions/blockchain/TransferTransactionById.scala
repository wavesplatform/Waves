package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testData.RandomDataGenerator.{randomAliasDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt}
import testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import utest.{Tests, test}

object TransferTransactionById extends JsTestBase {
  private val transferTransactionById = "transferTransactionById(callerTestData)"
  private val transferTransactionByIdArgBeforeFunc = "callerTestData.transferTransactionById()"

  private val invalidTransferTransactionById = "transferTransactionById()"
  private val invalidTransferTransactionByIdArg = s"$randomAliasDataArrayElement.transferTransactionById()"
  private val testData = new TestDataConstantsAndMethods


  val tests: Tests = Tests {
    test.apply("check: function transferTransactionById compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transferTransactionById
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function transferTransactionById (argument before function) compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transferTransactionByIdArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: transferTransactionById Non-matching type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          transferTransactionByIdArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: transferTransactionById Non-matching type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          invalidTransferTransactionByIdArg
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Function 'transferTransactionById' requires 1 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidTransferTransactionById
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("transferTransactionById", 1))
      }
    }
  }
}
