package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import _root_.testData.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt}
import utest.{Tests, test}

object IsDataStorageUntouched extends JsTestBase {
  val isDataStorageUntouched = "isDataStorageUntouched(callerTestData)"
  val isDataStorageUntouchedArgBeforeFunc = "callerTestData.isDataStorageUntouched()"
  val invalidFunction = "isDataStorageUntouched()"

  val testData = new TestDataConstantsAndMethods
  val invalidFunctionErrorResult: String = testData.invalidFunctionError("isDataStorageUntouched")

  val tests: Tests = Tests {
    test.apply("check: function isDataStorageUntouched compiles for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          isDataStorageUntouched,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function isDataStorageUntouched compiles (argument before function) for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          isDataStorageUntouchedArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function isDataStorageUntouched compiles for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAliasDataArrayElement,
          isDataStorageUntouched,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function isDataStorageUntouched compiles (argument before function) for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAliasDataArrayElement,
          isDataStorageUntouchedArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find function isDataStorageUntouched (argument before function) for address") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          isDataStorageUntouchedArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: Can't find function isDataStorageUntouched for alias") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAliasDataArrayElement,
          isDataStorageUntouched,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: invalid function isDataStorageUntouched for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAddressDataArrayElement,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
      }
    }

    test.apply("compilation error: invalid function isDataStorageUntouched for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomAliasDataArrayElement,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
      }
    }

    test.apply("compilation error: invalid data for function isDataStorageUntouched") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomInt.toString,
          isDataStorageUntouched,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.NON_MATCHING_TYPES)
      }
    }

    test.apply("compilation error: invalid data for function isDataStorageUntouched (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          randomInt.toString,
          isDataStorageUntouchedArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.NON_MATCHING_TYPES)
      }
    }
  }
}
