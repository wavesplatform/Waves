package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomDigestAlgorithmTypeArrayElement, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.GeneratorContractsForBuiltInFunctions
import utest.{Tests, test}

object CalculateLeaseId extends JsTestBase {
  private val calculateLeaseId              = "calculateLeaseId(lease)"
  private val calculateLeaseIdArgBeforeFunc = "lease.calculateLeaseId()"
  private val invalidCalculateLeaseId       = "calculateLeaseId()"

  val tests: Tests = Tests {
    test.apply("check: function calculateLeaseId for V5 and more compiles for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomAddressDataArrayElement,
          calculateLeaseId
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function calculateLeaseId for V5 and more (argument before function) compiles for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomAddressDataArrayElement,
          calculateLeaseIdArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function calculateLeaseId for V5 and more compiles for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomAliasDataArrayElement,
          calculateLeaseId
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function calculateLeaseId for V5 and more (argument before function) compiles for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomAliasDataArrayElement,
          calculateLeaseIdArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: calculateLeaseId Can't find a function overload") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomStringArrayElement,
          calculateLeaseId
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: calculateLeaseId Can't find a function overload (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomDigestAlgorithmTypeArrayElement,
          calculateLeaseIdArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: calculateLeaseId Can't find a function overload") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomStringArrayElement,
          invalidCalculateLeaseId
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: function calculateLeaseId for V5 and more requires 1 argument") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.codeForCalculateLeaseId(
          randomDigestAlgorithmTypeArrayElement,
          invalidCalculateLeaseId
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("calculateLeaseId", 1))
      }
    }

    test.apply("compilation error: calculateLeaseId function is missing") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeForCalculateLeaseId(
          randomIssuesArrayElement,
          calculateLeaseId
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: calculateLeaseId (argument before function) function is missing") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeForCalculateLeaseId(
          randomIssuesArrayElement,
          calculateLeaseIdArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
