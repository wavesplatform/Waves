package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Bn256Groth16Verify extends JsTestBase {
  // bn256Groth16Verify
  private val bn256Groth16Verify                     = s"bn256Groth16Verify(callerTestData, callerTestData, callerTestData)"
  private val bn256Groth16VerifyArgBeforeFunc        = s"callerTestData.bn256Groth16Verify(callerTestData, callerTestData)"
  private val invalidBn256Groth16Verify              = "bn256Groth16Verify()"
  private val invalidBn256Groth16VerifyArgBeforeFunc = "callerTestData.bn256Groth16Verify(callerTestData)"
  private val invalidErrorBn256Groth16Verify         = testData.invalidFunctionError("bn256Groth16Verify", 3)

  val tests: Tests = Tests {
    test.apply("check: bn256Groth16Verify function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, bn256Groth16Verify)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: bn256Groth16Verify function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, bn256Groth16VerifyArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: bn256Groth16Verify - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, bn256Groth16Verify)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: bn256Groth16Verify - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, bn256Groth16VerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload bn256Groth16Verify") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidBn256Groth16Verify)
        assertCompileErrorDApp(script, version, invalidErrorBn256Groth16Verify)
      }
    }

    test.apply("compilation error: Can't find a function overload bn256Groth16Verify (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidBn256Groth16VerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorBn256Groth16Verify)
      }
    }

    test.apply("compilation error: Can't find a function bn256Groth16Verify") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, bn256Groth16Verify)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
