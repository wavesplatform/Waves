package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object TransactionHeightById extends JsTestBase {
  private val transactionHeightById = "transactionHeightById(callerTestData)"
  private val transactionHeightByIdArgBeforeFunc = "callerTestData.transactionHeightById()"

  private val invalidTransactionHeightById = "transactionHeightById()"
  private val invalidTransactionHeightByIdArg = s"callerTestData.transactionHeightById(callerTestData)"


  val tests: Tests = Tests {
    test("RIDE-42. TransactionHeightById function should compile") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, transactionHeightById),
            (randomByteVectorArrayElement, transactionHeightByIdArgBeforeFunc),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-43. Negative cases for TransactionHeightById function") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function, error) <- Seq(
            (randomDigestAlgorithmTypeArrayElement, transactionHeightById, nonMatchingTypes("ByteVector")),
            (randomStringArrayElement, transactionHeightByIdArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidTransactionHeightById, invalidFunctionError("transactionHeightById", 1)),
            (randomByteVectorArrayElement, invalidTransactionHeightByIdArg, invalidFunctionError("transactionHeightById", 1))
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
