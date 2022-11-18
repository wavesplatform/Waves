package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object Extract extends JsTestBase {
  // extract
  private val extract                     = "extract(callerTestData)"
  private val extractArgBeforeFunc        = "callerTestData.extract()"
  private val invalidExtract              = "extract()"
  private val invalidExtractArgBeforeFunc = "callerTestData.extract(callerTestData)"

  private val invalidErrorExtract         = testData.invalidFunctionError("extract", 1)

  val tests: Tests = Tests {
    test.apply("check: extract function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script       = precondition.onlyMatcherContract(randomInt.toString, extract)
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("check: extract function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script       = precondition.onlyMatcherContract(randomInt.toString, extractArgBeforeFunc)
      assertCompileSuccessDApp(script, V3)
    }

    test.apply("compilation error: extract - Non-matching types: expected") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script       = precondition.onlyMatcherContract(randomUnionArrayElement, extract)
      assertCompileErrorDApp(script, V3, testData.MATCHING_NOT_EXHAUSTIVE)

    }

    test.apply("compilation error: extract - Non-matching types: expected: (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script       = precondition.onlyMatcherContract(randomAddressDataArrayElement, extractArgBeforeFunc)
      assertCompileErrorDApp(script, V3, testData.MATCHING_NOT_EXHAUSTIVE)
    }

    test.apply("compilation error: Can't find a function overload extract") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.onlyMatcherContract(randomUnionArrayElement, invalidExtract)
      assertCompileErrorDApp(script, V3, invalidErrorExtract)
    }

    test.apply("compilation error: Can't find a function overload extract (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidExtractArgBeforeFunc)
      assertCompileErrorDApp(script, V3, invalidErrorExtract)
    }

    test.apply("check: extract function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, extract)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("check: extract function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, extractArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
