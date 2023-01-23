package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.thisVariable
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
    test("check: function getInteger compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getInteger,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getInteger compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getIntegerArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getInteger compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getInteger,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getInteger compiles (argument before function) for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getIntegerArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getInteger compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getInteger,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getInteger compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getIntegerArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getInteger compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeOwnData(
          ownDataGetInt,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getInteger (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeOwnData(
          ownDataGetIntArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getIntegerValue compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getIntegerValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getIntegerValue compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getIntegerValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getIntegerValue compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getIntegerValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getIntegerValue compiles (argument before function) for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getIntegerValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getIntegerValue compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getIntegerValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getIntegerValue compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getIntegerValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getIntegerValue (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeOwnData(
          ownDataGetIntValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getIntegerValue compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeOwnData(
          ownDataGetIntValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: Can't find a function overload getInteger") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          invalidGetInt,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload getIntegerValue") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          invalidGetIntValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getInteger") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          ownDataGetIntValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getIntegerValue") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          ownDataGetIntValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload getInteger - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getInteger,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload getIntegerValue - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getIntegerValue,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getInteger - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"getIntegerValue($randomInt)"
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getIntegerValue - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"$randomInt.getIntegerValue()"
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultIntegerEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
