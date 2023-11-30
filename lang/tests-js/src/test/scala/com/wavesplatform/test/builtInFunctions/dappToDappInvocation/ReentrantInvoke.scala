package com.wavesplatform.test.builtInFunctions.dappToDappInvocation

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{nonMatchingTypes, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object ReentrantInvoke extends JsTestBase {
  private val reentrantInvoke                = "reentrantInvoke(addressFromStringValue(dapp2),\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val reentrantInvokeArgBeforeFunc   = "addressFromStringValue(dapp2).reentrantInvoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidReentrantInvokeFunction = "reentrantInvoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidReentrantInvokeArgBeforeFunc = "addressFromStringValue(dapp2).reentrantInvoke([AttachedPayment(byteVector, payment)])"
  private val invalidFunctionErrorResult: String  = testData.invalidFunctionError("reentrantInvoke", 4)

  val tests: Tests = Tests {
    test("RIDE-90. ReentrantInvoke function should compile for Issue RIDE V5 and more") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (byteVector, data, function) <- Seq(
            (randomByteVectorArrayElement, randomInt.toString, reentrantInvoke),
            (randomByteVectorArrayElement, randomInt.toString, reentrantInvokeArgBeforeFunc)
          )
        ) {
          val script = precondition.codeForDAppInvocation(byteVector, data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-91. ReentrantInvoke function throws an error for invalid values for RIDE V5 and more") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (byteVector, data, function, error) <- Seq(
            (randomByteVectorArrayElement, randomAddressDataArrayElement, reentrantInvoke, nonMatchingTypes("Int")),
            (randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, reentrantInvokeArgBeforeFunc, nonMatchingTypes("Int")),
            (randomStringArrayElement, randomInt.toString, reentrantInvoke, nonMatchingTypes("ByteVector|Unit")),
            (randomStringArrayElement, randomInt.toString, reentrantInvokeArgBeforeFunc, nonMatchingTypes("ByteVector|Unit")),
            (randomByteVectorArrayElement, randomInt.toString, invalidReentrantInvokeFunction, invalidFunctionErrorResult),
            (randomByteVectorArrayElement, randomInt.toString, invalidReentrantInvokeArgBeforeFunc, invalidFunctionErrorResult)
          )
        ) {
          val script = precondition.codeForDAppInvocation(byteVector, data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
