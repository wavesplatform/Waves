package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt}
import testHelpers.TestDataConstantsAndMethods.thisVariable
import utest.{Tests, test}

object IsDataStorageUntouched extends JsTestBase {
  private val isDataStorageUntouched = "isDataStorageUntouched(callerTestData)"
  private val isDataStorageUntouchedArgBeforeFunc = "callerTestData.isDataStorageUntouched()"
  private val invalidFunction = "isDataStorageUntouched()"

  private val invalidFunctionErrorResult: String = testData.invalidFunctionError("isDataStorageUntouched", 1)

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

    test.apply("check: function isDataStorageUntouched compiles for 'this'") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          thisVariable,
          isDataStorageUntouched,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function isDataStorageUntouched compiles (argument before function) for 'this'") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeWithoutMatcher(
          thisVariable,
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
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
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
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }
  }
}
