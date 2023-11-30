package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object TransferTransactionFromProto extends JsTestBase {
  private val transferTransactionFromProto                         = "transferTransactionFromProto(callerTestData)"
  private val transferTransactionFromProtoArgBeforeFunc            = "callerTestData.transferTransactionFromProto()"
  private val invalidTransferTransactionFromProto                  = "transferTransactionFromProto()"
  private val invalidTransferTransactionFromProtoArgBeforeFunction = "transferTransactionFromProto()"

  val tests: Tests = Tests {
    test("RIDE-85. transferTransactionFromProto function should compile for Issue V4 and more") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, transferTransactionFromProto),
            (randomByteVectorArrayElement, transferTransactionFromProtoArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-86. transferTransactionFromProto function throws an error for invalid values for V4 and more") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        for (
          (data, function, error) <- Seq(
            (randomDigestAlgorithmTypeArrayElement, transferTransactionFromProto, nonMatchingTypes("ByteVector")),
            (randomDigestAlgorithmTypeArrayElement, transferTransactionFromProtoArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidTransferTransactionFromProto, invalidFunctionError("transferTransactionFromProto", 1)),
            (
              randomByteVectorArrayElement,
              invalidTransferTransactionFromProtoArgBeforeFunction,
              invalidFunctionError("transferTransactionFromProto", 1)
            )
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-87. transferTransactionFromProto function should throw a compilation error for Ride V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", V3)
      for (
        (data, function) <- Seq(
          (randomByteVectorArrayElement, transferTransactionFromProto),
          (randomByteVectorArrayElement, transferTransactionFromProtoArgBeforeFunc)
        )
      ) {
        val script = precondition.onlyMatcherContract(data, function)
        assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
    }
  }
}
