package com.wavesplatform.test.builtInFunctions.accountDataStorage

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt, randomStringArrayElement}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{GreaterV3ResultBinaryEntry, rideV3Result, thisVariable}
import utest.{Tests, test}

object GetBinary extends JsTestBase {
  // getBinary
  private val getBinary                     = s"getBinary(callerTestData, $randomStringArrayElement)"
  private val getBinaryArgBeforeFunc        = s"callerTestData.getBinary($randomStringArrayElement)"
  private val ownDataGetBinary              = s"getBinary($randomStringArrayElement)"
  private val ownDataGetBinaryArgBeforeFunc = s"$randomStringArrayElement.getBinary()"

  // getBinaryValue
  private val getBinaryValue                     = s"getBinaryValue(callerTestData, $randomStringArrayElement)"
  private val getBinaryValueArgBeforeFunc        = s"callerTestData.getBinaryValue($randomStringArrayElement)"
  private val ownDataGetBinaryValue              = s"getBinaryValue($randomStringArrayElement)"
  private val ownDataGetBinaryValueArgBeforeFunc = s"$randomStringArrayElement.getBinaryValue()"

  private val invalidGetBinary      = s"getBinary(callerTestData)"
  private val invalidGetBinaryValue = s"getBinaryValue(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-4. Compile getBinary functions for address, alias, and 'this'") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (addressOrAlias, binary) <- Seq(
            (randomAddressDataArrayElement, getBinary),
            (randomAddressDataArrayElement, getBinaryArgBeforeFunc),
            (randomAliasDataArrayElement, getBinary),
            (randomAliasDataArrayElement, getBinaryArgBeforeFunc),
            (randomAddressDataArrayElement, getBinaryValue),
            (randomAddressDataArrayElement, getBinaryValueArgBeforeFunc),
            (randomAliasDataArrayElement, getBinaryValue),
            (randomAliasDataArrayElement, getBinaryValueArgBeforeFunc),
            (thisVariable, getBinary),
            (thisVariable, getBinaryArgBeforeFunc),
            (thisVariable, getBinaryValue),
            (thisVariable, getBinaryValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-5. Compile own data getBinary functions for address, alias, and 'this'") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (ownData <- Seq(ownDataGetBinary, ownDataGetBinaryArgBeforeFunc, ownDataGetBinaryValueArgBeforeFunc, ownDataGetBinaryValue)) {
          val script = precondition.codeOwnData(ownData, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-6. Test negative scenarios for getBinary functions") {
      val invalidFunction = s"getBinaryValue($randomInt)"
      val invalidArgBeforeFunction = s"$randomInt.getBinaryValue()"
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (addressOrAlias, binary) <- Seq(
            (randomAddressDataArrayElement, invalidGetBinary),
            (randomAliasDataArrayElement, invalidGetBinaryValue),
            (randomInt.toString, getBinary),
            (randomInt.toString, getBinaryValue),
            (randomInt.toString, invalidFunction),
            (randomInt.toString, invalidArgBeforeFunction),
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }

    test("RIDE-7. Ensure no overload of own data Binary accountDataStorage for old versions") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (addressOrAlias, binary) <- Seq(
            (randomAddressDataArrayElement, ownDataGetBinaryValue),
            (randomAliasDataArrayElement, ownDataGetBinaryValueArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(addressOrAlias, binary, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
