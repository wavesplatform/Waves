package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.RandomDataGenerator.{randomDigestAlgorithmTypeArrayElement, randomIssuesArrayElement}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object CalculateAssetId extends JsTestBase {
  private val calculateAssetId              = "calculateAssetId(issue)"
  private val calculateAssetIdArgBeforeFunc = "issue.calculateAssetId()"
  private val invalidCalculateAssetId       = "calculateAssetId()"

  val tests: Tests = Tests {
    test("RIDE-33. CalculateAssetId function should compile for version V4 and higher when called for Issue operatio") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomIssuesArrayElement, calculateAssetId),
            (randomIssuesArrayElement, calculateAssetIdArgBeforeFunc)
          )
        ) {
          val script = precondition.codeForCalculateAssetId(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-34. Negative cases for CalculateAssetId function for version V4 and higher") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function, error) <- Seq(
            (randomDigestAlgorithmTypeArrayElement, calculateAssetId, nonMatchingTypes("Issue")),
            (randomDigestAlgorithmTypeArrayElement, calculateAssetIdArgBeforeFunc, nonMatchingTypes("Issue")),
            (randomDigestAlgorithmTypeArrayElement, invalidCalculateAssetId, invalidFunctionError("calculateAssetId", 1))
          )
        ) {
          val script = precondition.codeForCalculateAssetId(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-35. Negative cases for CalculateAssetId function for version V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      for (
        (data, function) <- Seq(
          (randomIssuesArrayElement, calculateAssetId),
          (randomIssuesArrayElement, calculateAssetIdArgBeforeFunc)
        )
      ) {
        val script = precondition.codeForCalculateAssetId(data, function)
        assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
    }
  }
}
