package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object CreateMerkleRoot extends JsTestBase {
  private val createMerkleRoot                     = s"createMerkleRoot([callerTestData], callerTestData, $randomInt)"
  private val createMerkleRootArgBeforeFunc        = s"[callerTestData].createMerkleRoot(callerTestData, $randomInt)"
  private val invalidCreateMerkleRoot              = "createMerkleRoot()"
  private val invalidCreateMerkleRootArgBeforeFunc = "[callerTestData].createMerkleRoot(callerTestData)"
  private val invalidErrorCreateMerkleRoot         = invalidFunctionError("createMerkleRoot", 3)

  val tests: Tests = Tests {
    test("RIDE-271. createMerkleRoot function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, createMerkleRoot),
            (randomByteVectorArrayElement, createMerkleRootArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-272. createMerkleRoot function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, createMerkleRoot, nonMatchingTypes("List[ByteVector]")),
            (randomAddressDataArrayElement, createMerkleRootArgBeforeFunc, nonMatchingTypes("List[ByteVector]")),
            (randomByteVectorArrayElement, invalidCreateMerkleRoot, invalidErrorCreateMerkleRoot),
            (randomByteVectorArrayElement, invalidCreateMerkleRootArgBeforeFunc, invalidErrorCreateMerkleRoot),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-273. Can't find a function createMerkleRoot") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, createMerkleRoot)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
