package com.wavesplatform.test.builtInFunctions.dappToDappInvocation

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object Invoke extends JsTestBase {
  private val invoke                             = "invoke(addressFromStringValue(dapp2),\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invokeArgBeforeFunc                = "addressFromStringValue(dapp2).invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidInvokeFunction              = "invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidInvokeArgBeforeFunc         = "addressFromStringValue(dapp2).invoke([AttachedPayment(byteVector, payment)])"
  private val invalidFunctionErrorResult: String = invalidFunctionError("invoke", 4)

  val tests: Tests = Tests {
    test("RIDE-88. Invoke function should compile for Issue RIDE V5 and more") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (byteVector, data, function) <- Seq(
            (randomByteVectorArrayElement, randomInt.toString, invoke),
            (randomByteVectorArrayElement, randomInt.toString, invokeArgBeforeFunc)
          )
        ) {
          val script = precondition.codeForDAppInvocation(byteVector, data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-89. Invoke function throws an error for invalid values for RIDE V5 and more") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (byteVector, data, function, error) <- Seq(
            (randomByteVectorArrayElement, randomAddressDataArrayElement, invoke, nonMatchingTypes("Int")),
            (randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, invokeArgBeforeFunc, nonMatchingTypes("Int")),
            (randomStringArrayElement, randomInt.toString, invoke, nonMatchingTypes("ByteVector|Unit")),
            (randomStringArrayElement, randomInt.toString, invokeArgBeforeFunc, nonMatchingTypes("ByteVector|Unit")),
            (randomByteVectorArrayElement, randomInt.toString, invalidInvokeFunction, invalidFunctionErrorResult),
            (randomByteVectorArrayElement, randomInt.toString, invalidInvokeArgBeforeFunc, invalidFunctionErrorResult)
          )
        ) {
          val script = precondition.codeForDAppInvocation(byteVector, data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
