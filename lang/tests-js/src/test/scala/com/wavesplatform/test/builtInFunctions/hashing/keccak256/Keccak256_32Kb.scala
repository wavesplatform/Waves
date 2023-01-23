package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Keccak256_32Kb extends JsTestBase {
  // keccak256_32Kb
  private val keccak256_32Kb                     = "keccak256_32Kb(callerTestData)"
  private val keccak256_32KbArgBeforeFunc        = "callerTestData.keccak256_32Kb()"
  private val invalidKeccak256_32Kb              = "keccak256_32Kb()"
  private val invalidKeccak256_32KbArgBeforeFunc = "callerTestData.keccak256_32Kb(callerTestData)"
  private val invalidErrorKeccak256_32Kb         = testData.invalidFunctionError("keccak256_32Kb", 1)

  val tests: Tests = Tests {
    test("check: keccak256_32Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_32Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: keccak256_32Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_32KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: keccak256_32Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, keccak256_32Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: keccak256_32Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, keccak256_32KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test("compilation error: Can't find a function overload keccak256_32Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_32Kb)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_32Kb)
      }
    }

    test("compilation error: Can't find a function overload keccak256_32Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_32KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_32Kb)
      }
    }
  }
}
