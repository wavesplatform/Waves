package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object Groth16Verify extends JsTestBase {
  private val groth16Verify                     = "groth16Verify(callerTestData, callerTestData, callerTestData)"
  private val groth16VerifyArgBeforeFunc        = "callerTestData.groth16Verify(callerTestData, callerTestData)"
  private val invalidGroth16Verify              = "groth16Verify()"
  private val invalidGroth16VerifyArgBeforeFunc = "callerTestData.groth16Verify()"
  private val invalidErrorGroth16Verify         = invalidFunctionError("groth16Verify", 3)

  val tests: Tests = Tests {
    test("RIDE-277. groth16Verify function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, groth16Verify),
            (randomByteVectorArrayElement, groth16VerifyArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-278. groth16Verify function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, groth16Verify, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, groth16VerifyArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidGroth16Verify, invalidErrorGroth16Verify),
            (randomByteVectorArrayElement, invalidGroth16VerifyArgBeforeFunc, invalidErrorGroth16Verify)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-279. Can't find a function groth16Verify") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, groth16Verify)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
