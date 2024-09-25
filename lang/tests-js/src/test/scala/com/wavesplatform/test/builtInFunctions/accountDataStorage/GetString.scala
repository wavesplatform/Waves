package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{
  GreaterV3ResultStringEntry,
  actualVersions,
  oldVersions,
  rideV3Result,
  thisVariable,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object GetString extends JsTestBase {
  // getString
  private val getString                     = s"getString(callerTestData, $randomStringArrayElement)"
  private val getStringArgBeforeFunc        = s"callerTestData.getString($randomStringArrayElement)"
  private val ownDataGetString              = s"getString($randomStringArrayElement)"
  private val ownDataGetStringArgBeforeFunc = s"$randomStringArrayElement.getString()"

  // getStringValue
  private val getStringValue                     = s"getStringValue(callerTestData, $randomStringArrayElement)"
  private val getStringValueArgBeforeFunc        = s"callerTestData.getStringValue($randomStringArrayElement)"
  private val ownDataGetStringValue              = s"getStringValue($randomStringArrayElement)"
  private val ownDataGetStringValueArgBeforeFunc = s"$randomStringArrayElement.getStringValue()"

  private val invalidGetString      = s"getString(callerTestData)"
  private val invalidGetStringValue = s"getStringValue(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-16. Compile getString functions for address, alias, and 'this'") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (addressOrAlias, stringData) <- Seq(
            (randomAddressDataArrayElement, getString),
            (randomAddressDataArrayElement, getStringArgBeforeFunc),
            (randomAliasDataArrayElement, getString),
            (randomAliasDataArrayElement, getStringArgBeforeFunc),
            (randomAddressDataArrayElement, getStringValue),
            (randomAddressDataArrayElement, getStringValueArgBeforeFunc),
            (randomAliasDataArrayElement, getStringValue),
            (randomAliasDataArrayElement, getStringValueArgBeforeFunc),
            (thisVariable, getString),
            (thisVariable, getStringArgBeforeFunc),
            (thisVariable, getStringValue),
            (thisVariable, getStringValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, stringData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-17. Compile own data getString functions for address, alias, and 'this'") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (ownData <- Seq(ownDataGetString, ownDataGetStringArgBeforeFunc, ownDataGetStringValueArgBeforeFunc, ownDataGetStringValue)) {
          val script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-18. Test negative scenarios for getString functions") {
      val invalidFunction          = s"getStringValue($randomInt)"
      val invalidArgBeforeFunction = s"$randomInt.getStringValue()"
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (addressOrAlias, stringData) <- Seq(
            (randomAddressDataArrayElement, invalidGetString),
            (randomAliasDataArrayElement, invalidGetStringValue),
            (randomInt.toString, getString),
            (randomInt.toString, getStringValue),
            (randomInt.toString, invalidFunction),
            (randomInt.toString, invalidArgBeforeFunction)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, stringData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-19. Ensure no overload of own data Integer accountDataStorage for old versions") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (addressOrAlias, stringData) <- Seq(
            (randomAddressDataArrayElement, ownDataGetStringValue),
            (randomAliasDataArrayElement, ownDataGetStringValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, stringData, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
