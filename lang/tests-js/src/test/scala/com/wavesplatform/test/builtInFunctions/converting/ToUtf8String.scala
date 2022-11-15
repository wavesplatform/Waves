package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object ToUtf8String extends JsTestBase {
  // toUtf8String
  private val toUtf8String = "toUtf8String(callerTestData)"
  private val toUtf8StringArgBeforeFunc = "callerTestData.toUtf8String()"
  private val invalidToUtf8String = "toUtf8String()"
  private val invalidToUtf8StringArgBeforeFunc = "callerTestData.toUtf8String(callerTestData)"

  val tests: Tests = Tests {
    test.apply("check: toUtf8String function compiles with a ByteVector data type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          toUtf8String,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toUtf8String function compiles with a string data type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          toUtf8StringArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload toUtf8String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          toUtf8String,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload toUtf8String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          toUtf8StringArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Function 'toUtf8String' requires 1 arguments toUtf8String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          invalidToUtf8String,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("toUtf8String", 1))
      }
    }

    test.apply("compilation error: Function 'toUtf8String' requires 1 arguments (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          invalidToUtf8StringArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("toUtf8String", 1))
      }
    }
  }
}