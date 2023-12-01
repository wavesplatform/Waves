package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultIntegerEntry, actualVersions, oldVersions, rideV3Result, thisVariable, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object GetInteger extends JsTestBase {
  // getInteger
  private val getInteger = s"getInteger(callerTestData, $randomStringArrayElement)"
  private val getIntegerArgBeforeFunc = s"callerTestData.getInteger($randomStringArrayElement)"
  private val ownDataGetInt = s"getInteger($randomStringArrayElement)"
  private val ownDataGetIntArgBeforeFunc = s"$randomStringArrayElement.getInteger()"

  // getIntegerValue
  private val getIntegerValue = s"getIntegerValue(callerTestData, $randomStringArrayElement)"
  private val getIntegerValueArgBeforeFunc = s"callerTestData.getIntegerValue($randomStringArrayElement)"
  private val ownDataGetIntValue = s"getIntegerValue($randomStringArrayElement)"
  private val ownDataGetIntValueArgBeforeFunc = s"$randomStringArrayElement.getIntegerValue()"

  private val invalidGetInt = s"getInteger(callerTestData)"
  private val invalidGetIntValue = s"getIntegerValue(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-12. Compile getInteger functions for address, alias, and 'this'") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (addressOrAlias, intData) <- Seq(
            (randomAddressDataArrayElement, getInteger),
            (randomAddressDataArrayElement, getIntegerArgBeforeFunc),
            (randomAliasDataArrayElement, getInteger),
            (randomAliasDataArrayElement, getIntegerArgBeforeFunc),
            (randomAddressDataArrayElement, getIntegerValue),
            (randomAddressDataArrayElement, getIntegerValueArgBeforeFunc),
            (randomAliasDataArrayElement, getIntegerValue),
            (randomAliasDataArrayElement, getIntegerValueArgBeforeFunc),
            (thisVariable, getInteger),
            (thisVariable, getIntegerArgBeforeFunc),
            (thisVariable, getIntegerValue),
            (thisVariable, getIntegerValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, intData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-13. Compile own data getInteger functions for address, alias, and 'this'") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (ownData <- Seq(ownDataGetInt, ownDataGetIntArgBeforeFunc, ownDataGetIntValueArgBeforeFunc, ownDataGetIntValue)) {
          val script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-14. Test negative scenarios for getInteger functions") {
      val invalidFunction = s"getIntegerValue($randomInt)"
      val invalidArgBeforeFunction = s"$randomInt.getIntegerValue()"
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (addressOrAlias, intData) <- Seq(
            (randomAddressDataArrayElement, invalidGetInt),
            (randomAliasDataArrayElement, invalidGetIntValue),
            (randomInt.toString, getInteger),
            (randomInt.toString, getIntegerValue),
            (randomInt.toString, invalidFunction),
            (randomInt.toString, invalidArgBeforeFunction),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, intData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-15. Ensure no overload of own data Integer accountDataStorage for old versions") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (addressOrAlias, intData) <- Seq(
            (randomAddressDataArrayElement, ownDataGetIntValue),
            (randomAliasDataArrayElement, ownDataGetIntValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, intData, rideV3Result, GreaterV3ResultIntegerEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
