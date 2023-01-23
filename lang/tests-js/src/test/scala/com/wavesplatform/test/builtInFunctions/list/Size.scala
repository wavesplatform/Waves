package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.intList
import utest.{Tests, test}

object Size extends JsTestBase {
  // size
  private val size                     = "size(callerTestData)"
  private val sizeArgBeforeFunc        = "callerTestData.size()"
  private val invalidSize              = "size()"
  private val invalidSizeArgBeforeFunc = "callerTestData.size(callerTestData)"

  val tests: Tests = Tests {
    test("check: size function compiles with a Int") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, size)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: size function compiles with a Int (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, sizeArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: size - Non-matching types: expected: Int") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, size)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: size - Non-matching types: expected: Int (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, sizeArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload size") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidSize)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload size (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script       = precondition.onlyMatcherContract(intList, invalidSizeArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }

}
