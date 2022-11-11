package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import com.wavesplatform.test.builtInFunctions.accountDataStorage.GetString.{assertCompileErrorDApp, assertCompileSuccessDApp}
import testData.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import utest.{Tests, test}

object GetString extends JsTestBase {
  // getString
  val getString = s"getString(callerTestData, $randomStringArrayElement)"
  val getStringArgBeforeFunc = s"callerTestData.getString($randomStringArrayElement)"
  val ownDataGetString = s"getString($randomStringArrayElement)"
  val ownDataGetStringArgBeforeFunc = s"$randomStringArrayElement.getString()"

  // getStringValue
  val getStringValue = s"getStringValue(callerTestData, $randomStringArrayElement)"
  val getStringValueArgBeforeFunc = s"callerTestData.getStringValue($randomStringArrayElement)"
  val ownDataGetStringValue = s"getStringValue($randomStringArrayElement)"
  val ownDataGetStringValueArgBeforeFunc = s"$randomStringArrayElement.getStringValue()"

  val invalidGetString = s"getString(callerTestData)"
  val invalidGetStringValue = s"getStringValue(callerTestData)"
  val testData = new TestDataConstantsAndMethods

  val tests: Tests = Tests {
    test.apply("check: function getString compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getString,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getString compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getStringArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getString compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getString,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getString compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getStringArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getString compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeOwnData(
          ownDataGetString,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getString (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeOwnData(
          ownDataGetStringArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getStringValue compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getStringValue,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getStringValue compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getStringValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getStringValue compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getStringValue,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getStringValue compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getStringValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getStringValue (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeOwnData(
          ownDataGetStringValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getStringValue compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeOwnData(
          ownDataGetStringValue,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload getString") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          invalidGetString,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload getStringValue") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          invalidGetStringValue,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getString") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          ownDataGetStringValue,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getStringValue") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          ownDataGetStringValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload getString - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getString,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload getStringValue - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getStringValue,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getString - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"getStringValue($randomInt)"
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getStringValue - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"$randomInt.getStringValue()"
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultStringEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
