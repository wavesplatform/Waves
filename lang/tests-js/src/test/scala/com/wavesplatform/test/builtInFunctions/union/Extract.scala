package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, MATCHING_NOT_EXHAUSTIVE, actualVersionsWithoutV3, invalidFunctionError}
import utest.{Tests, test}

object Extract extends JsTestBase {
  private val extract                     = "extract(callerTestData)"
  private val extractArgBeforeFunc        = "callerTestData.extract()"
  private val invalidExtract              = "extract()"
  private val invalidExtractArgBeforeFunc = "callerTestData.extract(callerTestData)"

  private val invalidErrorExtract = invalidFunctionError("extract", 1)

  val tests: Tests = Tests {
    test("Extract functions compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      for (
        (data, function) <- Seq(
          (randomInt.toString, extract),
          (randomInt.toString, extractArgBeforeFunc)
        )
      ) {
        val script = precondition.onlyMatcherContract(data, function)
        assertCompileSuccessDApp(script, V3)
      }
    }

    test("invalid extract functions") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      for (
        (data, function, error) <- Seq(
          (randomUnionArrayElement, extract, MATCHING_NOT_EXHAUSTIVE),
          (randomUnionArrayElement, extractArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE),
          (randomInt.toString, invalidExtract, invalidErrorExtract),
          (randomInt.toString, invalidExtractArgBeforeFunc, invalidErrorExtract),
        )
      ) {
        val script = precondition.onlyMatcherContract(data, function)
        assertCompileErrorDApp(script, V3, error)
      }
    }

    test("invalid extract functions for V4 - V6") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, extract),
            (randomInt.toString, extractArgBeforeFunc),
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
