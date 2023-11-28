package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, intList}
import utest.{Tests, test}

object Size extends JsTestBase {
  private val size                     = "size(callerTestData)"
  private val sizeArgBeforeFunc        = "callerTestData.size()"
  private val invalidSize              = "size()"
  private val invalidSizeArgBeforeFunc = "callerTestData.size(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-170. Function Size should compile for valid list") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (intList, size),
            (intList, sizeArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-171. Function Size should throw an error for invalid data or type") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomUnionArrayElement, size),
            (randomAddressDataArrayElement, sizeArgBeforeFunc),
            (intList, invalidSize),
            (intList, invalidSizeArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
