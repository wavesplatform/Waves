package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultStringEntry, actualVersions, invalidFunctionError, nonMatchingTypes, rideV3Result}
import utest.{Tests, test}

object ToUtf8String extends JsTestBase {
  private val toUtf8String = "toUtf8String(callerTestData)"
  private val toUtf8StringArgBeforeFunc = "callerTestData.toUtf8String()"
  private val invalidToUtf8String = "toUtf8String()"
  private val invalidToUtf8StringArgBeforeFunc = "callerTestData.toUtf8String(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-83. Functions toUtf8String function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for ((data, function) <- Seq(
            (randomByteVectorArrayElement, toUtf8String),
            (randomByteVectorArrayElement, toUtf8StringArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-84. toUtf8String function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for ((data, function, error) <- Seq(
            (randomUnionArrayElement, toUtf8String, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, toUtf8StringArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidToUtf8String, invalidFunctionError("toUtf8String", 1)),
            (randomByteVectorArrayElement, invalidToUtf8StringArgBeforeFunc, invalidFunctionError("toUtf8String", 1))
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}