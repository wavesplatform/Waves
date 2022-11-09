package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.StdLibVersion.VersionDic
import testData.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomStringArrayElement}
import testData.{GeneratorContractsForBuiltInFunctions, TestDataConstants}
import utest.{Tests, test}

object GetBinary extends JsTestBase {
  // getBinary
  val getBinary = s"getBinary(callerTestData, $randomStringArrayElement)"
  val getBinaryArgBeforeFunc = s"callerTestData.getBinary($randomStringArrayElement)"
  val ownDataGetBinary = s"getBinary($randomStringArrayElement)"
  val ownDataGetBinaryArgBeforeFunc = s"$randomStringArrayElement.getBinary()"

  // getBinaryValue
  val getBinaryValue = s"getBinaryValue(callerTestData, $randomStringArrayElement)"
  val getBinaryValueArgBeforeFunc = s"callerTestData.getBinaryValue($randomStringArrayElement)"
  val ownDataGetBinaryValue = s"getBinaryValue($randomStringArrayElement)"
  val ownDataGetBinaryValueArgBeforeFunc = s"$randomStringArrayElement.getBinaryValue()"

  val invalidGetBinary = s"getBinary(callerTestData)"
  val invalidGetBinaryValue = s"getBinaryValue(callerTestData)"
  val testData = new TestDataConstants

  val tests: Tests = Tests {
    test.apply("check function getBinary compiles for address") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAddressDataArrayElement,
          getBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinary compiles for alias") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAliasDataArrayElement,
          getBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinary compiles (argument before function) for address") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAddressDataArrayElement,
          getBinaryArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinary compiles (argument before function) for alias") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAliasDataArrayElement,
          getBinaryArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinaryValue compiles for address") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAddressDataArrayElement,
          getBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinaryValue compiles for alias") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAliasDataArrayElement,
          getBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinaryValue compiles (argument before function) for address") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAddressDataArrayElement,
          getBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinaryValue compiles (argument before function) for alias") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAliasDataArrayElement,
          getBinaryValueArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check function getBinary, error for overload function") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAddressDataArrayElement,
          invalidGetBinary,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("check function getBinaryValue, error for overload function") {
      for (version <- VersionDic.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.contractWithMatchingAndCase(
          randomAliasDataArrayElement,
          invalidGetBinaryValue,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )

        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

  }
}