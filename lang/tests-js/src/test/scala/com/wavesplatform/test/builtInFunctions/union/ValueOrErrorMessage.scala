package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomInt,
  randomStringArrayElement,
  randomUnionArrayElement
}
import utest.{Tests, test}

object ValueOrErrorMessage extends JsTestBase {
  private val valueOrErrorMessage                     = s"valueOrErrorMessage(callerTestData, \"error message\")"
  private val valueOrErrorMessageArgBeforeFunction    = s"callerTestData.valueOrErrorMessage(\"error message\")"
  private val invalidValueOrErrorMessage              = s"valueOrErrorMessage(callerTestData)"
  private val invalidValueOrErrorMessageArgBeforeFunc = s"callerTestData.valueOrErrorMessage(callerTestData, \"error message\")"
  private val valueOrErrorInvalidFunctionMessage      = testData.invalidFunctionError("valueOrErrorMessage", 2)

  val tests: Tests = Tests {
    test.apply("check: function valueOrErrorMessage compiles with Int") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, valueOrErrorMessage)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function valueOrErrorMessage compiles with Boolean") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomBoolean.toString, valueOrErrorMessage)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function valueOrErrorMessage compiles with Boolean") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.onlyMatcherContract(randomStringArrayElement, valueOrErrorMessage)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function valueOrErrorMessage compiles with ByteVector (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, valueOrErrorMessageArgBeforeFunction)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function valueOrErrorMessage compiles with Address (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Address", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, valueOrErrorMessageArgBeforeFunction)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function valueOrErrorMessage compiles with Alias (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Alias", version)
        val script       = precondition.onlyMatcherContract(randomAliasDataArrayElement, valueOrErrorMessageArgBeforeFunction)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Alias", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, valueOrErrorMessage)
        assertCompileErrorDApp(script, version, testData.MATCHING_NOT_EXHAUSTIVE)
      }
    }

    test.apply("compilation error: Can't find a function overload, invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, valueOrErrorMessageArgBeforeFunction)
        assertCompileErrorDApp(script, version, testData.MATCHING_NOT_EXHAUSTIVE)
      }
    }

    test.apply("compilation error: invalid function valueOrErrorMessage Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.onlyMatcherContract(randomStringArrayElement, invalidValueOrErrorMessage)
        assertCompileErrorDApp(script, version, valueOrErrorInvalidFunctionMessage)
      }
    }

    test.apply("compilation error: invalid function valueOrErrorMessage Can't find a function overload (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, invalidValueOrErrorMessageArgBeforeFunc)
        assertCompileErrorDApp(script, version, valueOrErrorInvalidFunctionMessage)
      }
    }
  }
}
