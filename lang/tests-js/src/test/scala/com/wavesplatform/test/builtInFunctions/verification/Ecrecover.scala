package com.wavesplatform.test.builtInFunctions.verification

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object Ecrecover extends JsTestBase {
  // ecrecover
  private val ecrecover                     = "ecrecover(callerTestData, callerTestData)"
  private val ecrecoverArgBeforeFunc        = "callerTestData.ecrecover(callerTestData)"
  private val invalidEcrecover              = "ecrecover()"
  private val invalidEcrecoverArgBeforeFunc = "callerTestData.ecrecover()"
  private val invalidErrorEcrecover         = testData.invalidFunctionError("ecrecover", 2)

  val tests: Tests = Tests {
    test.apply("check: ecrecover function compiles with a ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, ecrecover)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: ecrecover function compiles with a ByteVector(argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, ecrecoverArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: ecrecover - Non-matching types: expected: ByteVector") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, ecrecover)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: ecrecover - Non-matching types: expected: ByteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, ecrecoverArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Can't find a function overload ecrecover") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidEcrecover)
        assertCompileErrorDApp(script, version, invalidErrorEcrecover)
      }
    }

    test.apply("compilation error: Can't find a function overload ecrecover (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidEcrecoverArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorEcrecover)
      }
    }

    test.apply("compilation error: Can't find a function ecrecover") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script       = precondition.onlyMatcherContract(randomByteVectorArrayElement, ecrecover)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }

}
