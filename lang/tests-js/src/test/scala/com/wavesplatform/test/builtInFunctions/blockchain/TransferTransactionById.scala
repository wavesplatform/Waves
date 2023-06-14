package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomStringArrayElement
}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object TransferTransactionById extends JsTestBase {
  private val transferTransactionById              = "transferTransactionById(callerTestData)"
  private val transferTransactionByIdArgBeforeFunc = "callerTestData.transferTransactionById()"

  private val invalidTransferTransactionById    = "transferTransactionById()"
  private val invalidTransferTransactionByIdArg = s"callerTestData.transferTransactionById(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-44. TransactionHeightById function should compile") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, transferTransactionById),
            (randomByteVectorArrayElement, transferTransactionByIdArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-45. Negative cases for TransactionHeightById function") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Unit", version)
        for (
          (data, function, error) <- Seq(
            (randomDigestAlgorithmTypeArrayElement, transferTransactionById, nonMatchingTypes("ByteVector")),
            (randomStringArrayElement, transferTransactionByIdArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidTransferTransactionById, invalidFunctionError("transferTransactionById", 1)),
            (randomByteVectorArrayElement, invalidTransferTransactionByIdArg, invalidFunctionError("transferTransactionById", 1))
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
