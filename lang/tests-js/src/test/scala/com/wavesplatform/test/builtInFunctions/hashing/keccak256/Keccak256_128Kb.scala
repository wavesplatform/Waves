package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Keccak256_128Kb extends JsTestBase {
  // keccak256_128Kb
  private val keccak256_128Kb                     = "keccak256_128Kb(callerTestData)"
  private val keccak256_128KbArgBeforeFunc        = "callerTestData.keccak256_128Kb()"
  private val invalidKeccak256_128Kb              = "keccak256_128Kb()"
  private val invalidKeccak256_128KbArgBeforeFunc = "callerTestData.keccak256_128Kb(callerTestData)"
  private val invalidErrorKeccak256_128Kb         = testData.invalidFunctionError("keccak256_128Kb", 1)

  val tests: Tests = Tests {
    test("check: keccak256_128Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_128Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: keccak256_128Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_128KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: keccak256_128Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, keccak256_128Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: keccak256_128Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, keccak256_128KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload keccak256_128Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_128Kb)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_128Kb)
      }
    }

    test("compilation error: Can't find a function overload keccak256_128Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_128KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_128Kb)
      }
    }
  }
}
