package com.wavesplatform.test.builtInFunctions.dappToDappInvocation

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomByteVectorArrayElement,
  randomInt,
  randomStringArrayElement
}
import utest.{Tests, test}

object Invoke extends JsTestBase {
  private val invoke                             = "invoke(addressFromStringValue(dapp2),\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invokeArgBeforeFunc                = "addressFromStringValue(dapp2).invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidInvokeFunction              = "invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  private val invalidInvokeArgBeforeFunc         = "addressFromStringValue(dapp2).invoke([AttachedPayment(byteVector, payment)])"
  private val invalidFunctionErrorResult: String = testData.invalidFunctionError("invoke", 4)

  val tests: Tests = Tests {
    test.apply("check: function invoke compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomInt.toString, invoke)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function invoke compiles (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomInt.toString, invokeArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: invalid data invoke") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, invoke)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
      }
    }

    test.apply("compilation error: invalid data invoke (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, invokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
      }
    }

    test.apply("compilation error: invalid function invoke") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomStringArrayElement, invalidInvokeFunction)
        assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
      }
    }

    test.apply("compilation error: invalid function invoke (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomStringArrayElement, invalidInvokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
      }
    }
  }
}
