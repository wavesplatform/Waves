package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomDigestAlgorithmTypeArrayElement, randomByteVectorArrayElement}
import utest.{Tests, test}

object TransferTransactionFromProto extends JsTestBase {
  private val transferTransactionFromProto                         = "transferTransactionFromProto(callerTestData)"
  private val transferTransactionFromProtoArgBeforeFunc            = "callerTestData.transferTransactionFromProto()"
  private val invalidTransferTransactionFromProto                  = "transferTransactionFromProto()"
  private val invalidTransferTransactionFromProtoArgBeforeFunction = "transferTransactionFromProto()"

  val tests: Tests = Tests {
    test.apply("check: function transferTransactionFromProto for version V4 and more compiles for Issue") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transferTransactionFromProto
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function transferTransactionFromProto for version V4 and more (argument before function) compiles for Issue") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          transferTransactionFromProtoArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: function transferTransactionFromProto for V4 and more Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          transferTransactionFromProto
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function transferTransactionFromProto for V4 and more Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          transferTransactionFromProtoArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function transferTransactionFromProto for V4 and more Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          invalidTransferTransactionFromProto
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("transferTransactionFromProto", 1))
      }
    }

    test.apply("compilation error: function transferTransactionFromProto for V4 and more Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          invalidTransferTransactionFromProtoArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("transferTransactionFromProto", 1))
      }
    }

    test.apply("compilation error: transferTransactionFromProto for V3 function is missing") {
      val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", V3)
      val script = precondition.onlyMatcherContract(
        randomByteVectorArrayElement,
        transferTransactionFromProto
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test.apply("compilation error: transferTransactionFromProto for V3 (argument before function) function is missing") {
      val precondition = new GeneratorContractsForBuiltInFunctions("TransferTransaction", V3)
      val script = precondition.onlyMatcherContract(
        randomByteVectorArrayElement,
        transferTransactionFromProtoArgBeforeFunc
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
