package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object CreateMerkleRoot extends JsTestBase {
  // createMerkleRoot
  private val createMerkleRoot                     = s"createMerkleRoot([callerTestData], callerTestData, $randomInt)"
  private val createMerkleRootArgBeforeFunc        = s"[callerTestData].createMerkleRoot(callerTestData, $randomInt)"
  private val invalidCreateMerkleRoot              = "createMerkleRoot()"
  private val invalidCreateMerkleRootArgBeforeFunc = "[callerTestData].createMerkleRoot(callerTestData)"
  private val invalidErrorCreateMerkleRoot         = testData.invalidFunctionError("createMerkleRoot", 3)

  val tests: Tests = Tests {
    test.apply("check: createMerkleRoot function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, createMerkleRoot)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: createMerkleRoot function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, createMerkleRootArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: createMerkleRoot - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, createMerkleRoot)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[ByteVector]"))
      }
    }

    test.apply("compilation error: createMerkleRoot - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, createMerkleRootArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[ByteVector]"))
      }
    }

    test.apply("compilation error: Can't find a function overload createMerkleRoot") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidCreateMerkleRoot)
        assertCompileErrorDApp(script, version, invalidErrorCreateMerkleRoot)
      }
    }

    test.apply("compilation error: Can't find a function overload createMerkleRoot (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidCreateMerkleRootArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorCreateMerkleRoot)
      }
    }

    test.apply("compilation error: Can't find a function createMerkleRoot") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, createMerkleRoot)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
