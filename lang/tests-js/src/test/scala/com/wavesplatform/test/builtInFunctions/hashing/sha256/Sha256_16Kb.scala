package com.wavesplatform.test.builtInFunctions.hashing.sha256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Sha256_16Kb extends JsTestBase {
  // sha256_16Kb
  private val sha256_16Kb                     = "sha256_16Kb(callerTestData)"
  private val sha256_16KbArgBeforeFunc        = "callerTestData.sha256_16Kb()"
  private val invalidSha256_16Kb              = "sha256_16Kb()"
  private val invalidSha256_16KbArgBeforeFunc = "callerTestData.sha256_16Kb(callerTestData)"
  private val invalidErrorSha256_16Kb  = testData.invalidFunctionError("sha256_16Kb", 1)

  val tests: Tests = Tests {
    test.apply("check: sha256_16Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          sha256_16Kb,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: sha256_16Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          sha256_16KbArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: sha256_16Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          sha256_16Kb,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: sha256_16Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          sha256_16KbArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload sha256_16Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          invalidSha256_16Kb,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, invalidErrorSha256_16Kb)
      }
    }

    test.apply("compilation error: Can't find a function overload sha256_16Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          invalidSha256_16KbArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, invalidErrorSha256_16Kb)
      }
    }
  }
}
