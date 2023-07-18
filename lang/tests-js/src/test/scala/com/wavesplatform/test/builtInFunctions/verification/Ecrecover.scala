package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object Ecrecover extends JsTestBase {
  private val ecrecover                     = "ecrecover(callerTestData, callerTestData)"
  private val ecrecoverArgBeforeFunc        = "callerTestData.ecrecover(callerTestData)"
  private val invalidEcrecover              = "ecrecover()"
  private val invalidEcrecoverArgBeforeFunc = "callerTestData.ecrecover()"
  private val invalidErrorEcrecover         = invalidFunctionError("ecrecover", 2)

  val tests: Tests = Tests {
    test("RIDE-274. ecrecover function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, ecrecover),
            (randomByteVectorArrayElement, ecrecoverArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-275. ecrecover function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, ecrecover, nonMatchingTypes("ByteVector")),
            (randomAddressDataArrayElement, ecrecoverArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidEcrecover, invalidErrorEcrecover),
            (randomByteVectorArrayElement, invalidEcrecoverArgBeforeFunc, invalidErrorEcrecover),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-276. Can't find a function ecrecover") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.onlyMatcherContract(randomByteVectorArrayElement, ecrecover)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
