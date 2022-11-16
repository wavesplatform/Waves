package com.wavesplatform.test.builtInFunctions.hashing.blake2b256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Blake2b256_16Kb extends JsTestBase {
  // blake2b256_16Kb
  private val blake2b256_16Kb                     = "blake2b256_16Kb(callerTestData)"
  private val blake2b256_16KbArgBeforeFunc        = "callerTestData.blake2b256_16Kb()"
  private val invalidBlake2b256_16Kb              = "blake2b256_16Kb()"
  private val invalidBlake2b256_16KbArgBeforeFunc = "callerTestData.blake2b256_16Kb(callerTestData)"
  private val invalidErrorBlake2b256_16Kb  = testData.invalidFunctionError("blake2b256_16Kb", 1)

  val tests: Tests = Tests {
    test.apply("check: blake2b256_16Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          blake2b256_16Kb,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: blake2b256_16Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomByteVectorArrayElement,
          blake2b256_16KbArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: blake2b256_16Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          blake2b256_16Kb,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: blake2b256_16Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          blake2b256_16KbArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload blake2b256_16Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          invalidBlake2b256_16Kb,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, invalidErrorBlake2b256_16Kb)
      }
    }

    test.apply("compilation error: Can't find a function overload blake2b256_16Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomUnionArrayElement,
          invalidBlake2b256_16KbArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, invalidErrorBlake2b256_16Kb)
      }
    }
  }
}
