package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomIssuesArrayElement,
  randomStringArrayElement
}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_A_FUNCTION_OVERLOAD,
  CANT_FIND_FUNCTION,
  invalidFunctionError,
  oldVersions,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object CalculateLeaseId extends JsTestBase {
  private val calculateLeaseId              = "calculateLeaseId(lease)"
  private val calculateLeaseIdArgBeforeFunc = "lease.calculateLeaseId()"
  private val invalidCalculateLeaseId       = "calculateLeaseId()"

  val tests: Tests = Tests {
    test("RIDE-36. calculateLeaseId function should compile for version V5 and higher when called for an address") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, calculateLeaseId),
            (randomAddressDataArrayElement, calculateLeaseIdArgBeforeFunc),
            (randomAliasDataArrayElement, calculateLeaseId),
            (randomAliasDataArrayElement, calculateLeaseIdArgBeforeFunc)
          )
        ) {
          val script = precondition.codeForCalculateLeaseId(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-37. Negative cases for calculateLeaseId function for version V5 and higher") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function, error) <- Seq(
            (randomStringArrayElement, calculateLeaseId, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomDigestAlgorithmTypeArrayElement, calculateLeaseIdArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomAddressDataArrayElement, invalidCalculateLeaseId, invalidFunctionError("calculateLeaseId", 1)),
            (randomAliasDataArrayElement, invalidCalculateLeaseId, invalidFunctionError("calculateLeaseId", 1))
          )
        ) {
          val script = precondition.codeForCalculateLeaseId(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-38. Negative cases for calculateLeaseId function for versions V3 and V4") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomIssuesArrayElement, calculateLeaseId),
            (randomIssuesArrayElement, calculateLeaseIdArgBeforeFunc)
          )
        ) {
          val script = precondition.codeForCalculateLeaseId(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
