package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.RandomDataGenerator.{randomIssuesArrayElement, randomDigestAlgorithmTypeArrayElement}
import testHelpers.GeneratorContractsForBuiltInFunctions
import utest.{Tests, test}

object CalculateAssetId extends JsTestBase {
  private val calculateAssetId              = "calculateAssetId(issue)"
  private val calculateAssetIdArgBeforeFunc = "issue.calculateAssetId()"
  private val invalidCalculateAssetId       = "calculateAssetId()"

  val tests: Tests = Tests {
    test("check: function calculateAssetId for version V4 and more compiles for Issue") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateAssetId(
          randomIssuesArrayElement,
          calculateAssetId
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function calculateAssetId for version V4 and more (argument before function) compiles for Issue") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateAssetId(
          randomIssuesArrayElement,
          calculateAssetIdArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: function calculateAssetId for V4 and more Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateAssetId(
          randomDigestAlgorithmTypeArrayElement,
          calculateAssetId
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Issue"))
      }
    }

    test("compilation error: function calculateAssetId for V4 and more Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateAssetId(
          randomDigestAlgorithmTypeArrayElement,
          calculateAssetIdArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Issue"))
      }
    }

    test("compilation error: function calculateAssetId for V4 and more requires 1 argument") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateAssetId(
          randomDigestAlgorithmTypeArrayElement,
          invalidCalculateAssetId
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("calculateAssetId", 1))
      }
    }

    test("compilation error: calculateAssetId for V3 function is missing") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeForCalculateAssetId(
        randomIssuesArrayElement,
        calculateAssetId
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: calculateAssetId for V3 (argument before function) function is missing") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V3)
      val script = precondition.codeForCalculateAssetId(
        randomIssuesArrayElement,
        calculateAssetIdArgBeforeFunc
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
