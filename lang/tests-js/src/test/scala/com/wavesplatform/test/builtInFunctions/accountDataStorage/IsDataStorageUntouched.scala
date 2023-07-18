package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt}
import testHelpers.TestDataConstantsAndMethods.{
  GreaterV3ResultBooleanEntry,
  invalidFunctionError,
  nonMatchingTypes,
  oldVersions,
  rideV3Result,
  thisVariable,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object IsDataStorageUntouched extends JsTestBase {
  private val isDataStorageUntouched              = "isDataStorageUntouched(callerTestData)"
  private val isDataStorageUntouchedArgBeforeFunc = "callerTestData.isDataStorageUntouched()"
  private val invalidFunction                     = "isDataStorageUntouched()"
  private val invalidFunctionErrorResult: String  = invalidFunctionError("isDataStorageUntouched", 1)

  val tests: Tests = Tests {
    test("RIDE-20. Compile isDataStorageUntouched functions for address, alias, and 'this'") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, dataStorage) <- Seq(
            (randomAddressDataArrayElement, isDataStorageUntouched),
            (randomAddressDataArrayElement, isDataStorageUntouchedArgBeforeFunc),
            (randomAliasDataArrayElement, isDataStorageUntouched),
            (randomAliasDataArrayElement, isDataStorageUntouchedArgBeforeFunc),
            (thisVariable, isDataStorageUntouched),
            (thisVariable, isDataStorageUntouchedArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-21. Non-matching types for function isDataStorageUntouched") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, dataStorage) <- Seq(
            (randomInt.toString, isDataStorageUntouched),
            (randomInt.toString, isDataStorageUntouchedArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, nonMatchingTypes("Address|Alias"))
        }
      }
    }

    test("RIDE-22. Invalid data for functions isDataStorageUntouched") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, dataStorage) <- Seq(
            (randomAddressDataArrayElement, invalidFunction),
            (randomAliasDataArrayElement, invalidFunction),
            (thisVariable, invalidFunction)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, invalidFunctionErrorResult)
        }
      }
    }

    test("RIDE-23. Can't find functions isDataStorageUntouched dataStorage accountDataStorage for old Versions") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, dataStorage) <- Seq(
            (randomAddressDataArrayElement, isDataStorageUntouchedArgBeforeFunc),
            (thisVariable, isDataStorageUntouchedArgBeforeFunc),
            (randomAliasDataArrayElement, isDataStorageUntouched)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, dataStorage, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
