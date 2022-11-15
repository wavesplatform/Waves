package com.wavesplatform.test.builtInFunctions.dappToDappInvocation

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomStringArrayElement}
import utest.{Tests, test}

object ReentrantInvoke extends JsTestBase {
  private val reentrantInvoke                = "reentrantInvoke(addressFromStringValue(dapp2),\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val reentrantInvokeArgBeforeFunc   = "addressFromStringValue(dapp2).reentrantInvoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidReentrantInvokeFunction = "reentrantInvoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidReentrantInvokeArgBeforeFunc = "addressFromStringValue(dapp2).reentrantInvoke([AttachedPayment(byteVector, payment)])"
  private val invalidFunctionErrorResult: String  = testData.invalidFunctionError("reentrantInvoke", 4)

  val tests: Tests = Tests {
    test.apply("check: function reentrantInvoke compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomInt.toString, reentrantInvoke)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function reentrantInvoke compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomInt.toString, reentrantInvokeArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: invalid data reentrantInvoke") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, reentrantInvoke)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
      }
    }

    test.apply("compilation error: invalid data reentrantInvoke (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, reentrantInvokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
      }
    }

    test.apply("compilation error: invalid function reentrantInvoke") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomStringArrayElement, invalidReentrantInvokeFunction)
        assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
      }
    }

    test.apply("compilation error: invalid function reentrantInvoke (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomStringArrayElement, invalidReentrantInvokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
      }
    }
  }
}
