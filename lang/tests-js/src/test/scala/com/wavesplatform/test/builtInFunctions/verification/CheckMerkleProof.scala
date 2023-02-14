package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, MATCHING_NOT_EXHAUSTIVE}
import utest.{Tests, test}

object CheckMerkleProof extends JsTestBase {
  private val checkMerkleProof                     = "checkMerkleProof(callerTestData, callerTestData, callerTestData)"
  private val checkMerkleProofArgBeforeFunc        = "callerTestData.checkMerkleProof(callerTestData, callerTestData)"
  private val invalidCheckMerkleProof              = "checkMerkleProof()"
  private val invalidCheckMerkleProofArgBeforeFunc = "callerTestData.checkMerkleProof(callerTestData)"

  private val invalidErrorCheckMerkleProof = testData.invalidFunctionError("checkMerkleProof", 3)

  val tests: Tests = Tests {
    test("checkMerkleProof functions compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      for (
        (data, function) <- Seq(
          (randomByteVectorArrayElement, checkMerkleProof),
          (randomByteVectorArrayElement, checkMerkleProofArgBeforeFunc)
        )
      ) {
        val script = precondition.onlyMatcherContract(data, function)
        assertCompileSuccessDApp(script, V3)
      }
    }

    test("invalid functions checkMerkleProof") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      for (
        (data, function, error) <- Seq(
          (randomUnionArrayElement, checkMerkleProof, MATCHING_NOT_EXHAUSTIVE),
          (randomAddressDataArrayElement, checkMerkleProofArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE),
          (randomByteVectorArrayElement, invalidCheckMerkleProof, invalidErrorCheckMerkleProof),
          (randomByteVectorArrayElement, invalidCheckMerkleProofArgBeforeFunc, invalidErrorCheckMerkleProof)
        )
      ) {
        val script = precondition.onlyMatcherContract(data, function)
        assertCompileErrorDApp(script, V3, error)
      }
    }

    test("check: checkMerkleProof functions compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, checkMerkleProof),
            (randomByteVectorArrayElement, checkMerkleProofArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
