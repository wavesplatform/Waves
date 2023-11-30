package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBooleanEntry, actualVersions, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object GetBoolean extends JsTestBase {
  // getBoolean
  private val getBoolean = s"getBoolean(callerTestData, $randomStringArrayElement)"
  private val getBooleanArgBeforeFunc = s"callerTestData.getBoolean($randomStringArrayElement)"
  private val ownDataGetBoolean = s"getBoolean($randomStringArrayElement)"
  private val ownDataGetBooleanArgBeforeFunc = s"$randomStringArrayElement.getBoolean()"

  // getBooleanValue
  private val getBooleanValue = s"getBooleanValue(callerTestData, $randomStringArrayElement)"
  private val getBooleanValueArgBeforeFunc = s"callerTestData.getBooleanValue($randomStringArrayElement)"
  private val ownDataGetBooleanValue = s"getBooleanValue($randomStringArrayElement)"
  private val ownDataGetBooleanValueArgBeforeFunc = s"$randomStringArrayElement.getBooleanValue()"

  private val invalidGetBoolean = s"getBoolean(callerTestData)"
  private val invalidGetBooleanValue = s"getBooleanValue(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-8. Compile getBoolean functions for address, alias, and 'this'") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, booleanData) <- Seq(
            (randomAddressDataArrayElement, getBoolean),
            (randomAddressDataArrayElement, getBooleanArgBeforeFunc),
            (randomAliasDataArrayElement, getBoolean),
            (randomAliasDataArrayElement, getBooleanArgBeforeFunc),
            (randomAddressDataArrayElement, getBooleanValue),
            (randomAddressDataArrayElement, getBooleanValueArgBeforeFunc),
            (randomAliasDataArrayElement, getBooleanValue),
            (randomAliasDataArrayElement, getBooleanValueArgBeforeFunc),
            (thisVariable, getBoolean),
            (thisVariable, getBooleanArgBeforeFunc),
            (thisVariable, getBooleanValue),
            (thisVariable, getBooleanValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, booleanData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-9. Compile own data getBoolean functions for address, alias, and 'this'") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (ownData <- Seq(ownDataGetBoolean, ownDataGetBooleanArgBeforeFunc, ownDataGetBooleanValueArgBeforeFunc, ownDataGetBooleanValue)) {
          val script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-10. Negative tests for getBoolean functions") {
      val invalidFunction = s"getBooleanValue($randomInt)"
      val invalidArgBeforeFunction = s"$randomInt.getBooleanValue()"
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, booleanData) <- Seq(
            (randomAddressDataArrayElement, invalidGetBoolean),
            (randomAliasDataArrayElement, invalidGetBooleanValue),
            (randomInt.toString, getBoolean),
            (randomInt.toString, getBooleanValue),
            (randomInt.toString, invalidFunction),
            (randomInt.toString, invalidArgBeforeFunction),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, booleanData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-11. Ensure no overload of own data Boolean accountDataStorage for old Versions") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (addressOrAlias, booleanData) <- Seq(
            (randomAddressDataArrayElement, ownDataGetBooleanValue),
            (randomAliasDataArrayElement, ownDataGetBooleanValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, booleanData, rideV3Result, GreaterV3ResultBooleanEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
