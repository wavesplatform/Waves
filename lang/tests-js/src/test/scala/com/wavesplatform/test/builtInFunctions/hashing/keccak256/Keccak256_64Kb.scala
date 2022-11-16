package com.wavesplatform.test.builtInFunctions.hashing.keccak256

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Keccak256_64Kb extends JsTestBase {
  // keccak256_64Kb
  private val keccak256_64Kb                     = "keccak256_64Kb(callerTestData)"
  private val keccak256_64KbArgBeforeFunc        = "callerTestData.keccak256_64Kb()"
  private val invalidKeccak256_64Kb              = "keccak256_64Kb()"
  private val invalidKeccak256_64KbArgBeforeFunc = "callerTestData.keccak256_64Kb(callerTestData)"
  private val invalidErrorKeccak256_64Kb         = testData.invalidFunctionError("keccak256_64Kb", 1)

  val tests: Tests = Tests {
    test.apply("check: keccak256_64Kb function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_64Kb)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: keccak256_64Kb function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, keccak256_64KbArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: keccak256_64Kb - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, keccak256_64Kb)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: keccak256_64Kb - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, keccak256_64KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload keccak256_64Kb") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_64Kb)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_64Kb)
      }
    }

    test.apply("compilation error: Can't find a function overload keccak256_64Kb (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidKeccak256_64KbArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorKeccak256_64Kb)
      }
    }
  }
}
