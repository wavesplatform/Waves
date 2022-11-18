package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object CheckMerkleProof extends JsTestBase {
  // checkMerkleProof
  private val checkMerkleProof                     = "checkMerkleProof(callerTestData, callerTestData, callerTestData)"
  private val checkMerkleProofArgBeforeFunc        = "callerTestData.checkMerkleProof(callerTestData, callerTestData)"
  private val invalidCheckMerkleProof              = "checkMerkleProof()"
  private val invalidCheckMerkleProofArgBeforeFunc = "callerTestData.checkMerkleProof(callerTestData)"

  private val invalidErrorCheckMerkleProof         = testData.invalidFunctionError("checkMerkleProof", 3)

  val tests: Tests = Tests {
    test.apply("check: checkMerkleProof function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, checkMerkleProof)
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: checkMerkleProof function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, checkMerkleProofArgBeforeFunc)
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("compilation error: checkMerkleProof - Non-matching types: expected") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomUnionArrayElement, checkMerkleProof)
      assertCompileErrorDApp(script, V3, testData.MATCHING_NOT_EXHAUSTIVE)

    }

    test.apply("compilation error: checkMerkleProof - Non-matching types: expected: (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomAddressDataArrayElement, checkMerkleProofArgBeforeFunc)
      assertCompileErrorDApp(script, V3, testData.MATCHING_NOT_EXHAUSTIVE)
    }

    test.apply("compilation error: Can't find a function overload checkMerkleProof") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidCheckMerkleProof)
      assertCompileErrorDApp(script, V3, invalidErrorCheckMerkleProof)
    }

    test.apply("compilation error: Can't find a function overload checkMerkleProof (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", V3)
      val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidCheckMerkleProofArgBeforeFunc)
      assertCompileErrorDApp(script, V3, invalidErrorCheckMerkleProof)
    }

    test.apply("check: checkMerkleProof function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, checkMerkleProof)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("check: checkMerkleProof function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, checkMerkleProofArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
