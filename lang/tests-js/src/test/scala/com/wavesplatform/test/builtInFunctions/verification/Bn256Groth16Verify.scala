package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object Bn256Groth16Verify extends JsTestBase {
  private val bn256Groth16Verify                     = s"bn256Groth16Verify(callerTestData, callerTestData, callerTestData)"
  private val bn256Groth16VerifyArgBeforeFunc        = s"callerTestData.bn256Groth16Verify(callerTestData, callerTestData)"
  private val invalidBn256Groth16Verify              = "bn256Groth16Verify()"
  private val invalidBn256Groth16VerifyArgBeforeFunc = "callerTestData.bn256Groth16Verify(callerTestData)"
  private val invalidErrorBn256Groth16Verify         = testData.invalidFunctionError("bn256Groth16Verify", 3)

  val tests: Tests = Tests {
    test("RIDE-265. bn256Groth16Verify function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, bn256Groth16Verify),
            (randomByteVectorArrayElement, bn256Groth16VerifyArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-266. bn256Groth16Verify function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, bn256Groth16Verify, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, bn256Groth16VerifyArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidBn256Groth16Verify, invalidErrorBn256Groth16Verify),
            (randomByteVectorArrayElement, invalidBn256Groth16VerifyArgBeforeFunc, invalidErrorBn256Groth16Verify)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-267. Can't find a function bn256Groth16Verify") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, bn256Groth16Verify)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
