package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.thisVariable
import utest.{Tests, test}

object GetBinary extends JsTestBase {
  // getBinary
  private val getBinary = s"getBinary(callerTestData, $randomStringArrayElement)"
  private val getBinaryArgBeforeFunc = s"callerTestData.getBinary($randomStringArrayElement)"
  private val ownDataGetBinary = s"getBinary($randomStringArrayElement)"
  private val ownDataGetBinaryArgBeforeFunc = s"$randomStringArrayElement.getBinary()"

  // getBinaryValue
  private val getBinaryValue = s"getBinaryValue(callerTestData, $randomStringArrayElement)"
  private val getBinaryValueArgBeforeFunc = s"callerTestData.getBinaryValue($randomStringArrayElement)"
  private val ownDataGetBinaryValue = s"getBinaryValue($randomStringArrayElement)"
  private val ownDataGetBinaryValueArgBeforeFunc = s"$randomStringArrayElement.getBinaryValue()"

  private val invalidGetBinary = s"getBinary(callerTestData)"
  private val invalidGetBinaryValue = s"getBinaryValue(callerTestData)"


  val tests: Tests = Tests {
    test.apply("check: function getBinary accountDataStorage compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary accountDataStorage compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBinaryArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary accountDataStorage compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary accountDataStorage compiles (argument before function) for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBinaryArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary accountDataStorage compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinary accountDataStorage compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBinaryArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getBinary accountDataStorage compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeOwnData(
          ownDataGetBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getBinary accountDataStorage (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeOwnData(
          ownDataGetBinaryArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValue accountDataStorage compiles for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValue accountDataStorage compiles (argument before function) for address") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          getBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValue accountDataStorage compiles for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValue accountDataStorage compiles (argument before function) for 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          thisVariable,
          getBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValue accountDataStorage compiles for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function getBinaryValue accountDataStorage compiles (argument before function) for alias") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          getBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getBinaryValue accountDataStorage (argument before function) compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeOwnData(
          ownDataGetBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function own data getBinaryValue accountDataStorage compiles") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeOwnData(
          ownDataGetBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload getBinary accountDataStorage") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          invalidGetBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload getBinaryValue accountDataStorage") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          invalidGetBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getBinary accountDataStorage") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          ownDataGetBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getBinaryValue accountDataStorage") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          ownDataGetBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload getBinary - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload getBinaryValue - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          getBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getBinary - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"getBinaryValue($randomInt)"
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a own data function overload getBinaryValue - invalid data") {
      for (version <- testData.actualVersions) {
        val invalidFunction = s"$randomInt.getBinaryValue()"
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomInt.toString,
          invalidFunction,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}