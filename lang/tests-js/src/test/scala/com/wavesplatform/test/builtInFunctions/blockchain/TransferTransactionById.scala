package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt}
import testHelpers.GeneratorContractsForBuiltInFunctions
import utest.{Tests, test}

object TransferTransactionById extends JsTestBase {
  private val transferTransactionById = "transferTransactionById(callerTestData)"
  private val transferTransactionByIdArgBeforeFunc = "callerTestData.transferTransactionById()"

  private val invalidTransferTransactionById = "transferTransactionById()"
  private val invalidTransferTransactionByIdArg = s"$randomAliasDataArrayElement.transferTransactionById()"

  val tests: Tests = Tests {
    test("check: function transferTransactionById compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transferTransactionById
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function transferTransactionById (argument before function) compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transferTransactionByIdArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: transferTransactionById Non-matching type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          transferTransactionById
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: transferTransactionById Non-matching type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          invalidTransferTransactionByIdArg
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Function 'transferTransactionById' requires 1 arguments") {
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
