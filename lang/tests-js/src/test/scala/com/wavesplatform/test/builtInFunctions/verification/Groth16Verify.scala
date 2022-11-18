package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Groth16Verify extends JsTestBase {
  // groth16Verify
  private val groth16Verify                     = "groth16Verify(callerTestData, callerTestData, callerTestData)"
  private val groth16VerifyArgBeforeFunc        = "callerTestData.groth16Verify(callerTestData, callerTestData)"
  private val invalidGroth16Verify              = "groth16Verify()"
  private val invalidGroth16VerifyArgBeforeFunc = "callerTestData.groth16Verify()"
  private val invalidErrorGroth16Verify         = testData.invalidFunctionError("groth16Verify", 3)

  val tests: Tests = Tests {
    test.apply("check: groth16Verify function compiles with a Boolean") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, groth16Verify)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: groth16Verify function compiles with a Boolean (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, groth16VerifyArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: groth16Verify - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, groth16Verify)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: groth16Verify - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, groth16VerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload groth16Verify") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidGroth16Verify)
        assertCompileErrorDApp(script, version, invalidErrorGroth16Verify)
      }
    }

    test.apply("compilation error: Can't find a function overload groth16Verify (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidGroth16VerifyArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorGroth16Verify)
      }
    }

    test.apply("compilation error: Can't find a function groth16Verify") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, groth16Verify)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
