package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.thisVariable
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
    test("check: function getBoolean compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBoolean,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBoolean compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBooleanArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBoolean compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBoolean,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBoolean compiles (argument before function) for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBooleanArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBoolean compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBoolean,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBoolean compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBooleanArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getBoolean compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeOwnData(
          ownDataGetBoolean,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getBoolean (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeOwnData(
          ownDataGetBooleanArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBooleanValue compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBooleanValue compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBooleanValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBooleanValue compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBooleanValue compiles (argument before function) for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBooleanValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBooleanValue compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function getBooleanValue compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBooleanValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getBooleanValue (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeOwnData(
          ownDataGetBooleanValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function own data getBooleanValue compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeOwnData(
          ownDataGetBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: Can't find a function overload getBoolean") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          invalidGetBoolean,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload getBooleanValue") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          invalidGetBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getBoolean") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          ownDataGetBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getBooleanValue") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          ownDataGetBooleanValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload getBoolean - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getBoolean,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload getBooleanValue - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getBooleanValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getBoolean - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"getBooleanValue($randomInt)"
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a own data function overload getBooleanValue - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"$randomInt.getBooleanValue()"
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultBooleanEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
