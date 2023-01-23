package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Keccak256_16Kb extends JsTestBase {
  // keccak256_16Kb
  private val keccak256_16Kb                     = "keccak256_16Kb(callerTestData)"
  private val keccak256_16KbArgBeforeFunc        = "callerTestData.keccak256_16Kb()"
  private val invalidKeccak256_16Kb              = "keccak256_16Kb()"
  private val invalidKeccak256_16KbArgBeforeFunc = "callerTestData.keccak256_16Kb(callerTestData)"
  private val invalidErrorKeccak256_16Kb         = testData.invalidFunctionError("keccak256_16Kb", 1)

  val tests: Tests = Tests {
    test("check: keccak256_16Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_16Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: keccak256_16Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_16KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: keccak256_16Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, keccak256_16Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: keccak256_16Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, keccak256_16KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload keccak256_16Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_16Kb)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_16Kb)
      }
    }

    test("compilation error: Can't find a function overload keccak256_16Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_16KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_16Kb)
      }
    }
  }
}
