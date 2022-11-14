package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testData.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomIssuesArrayElement,
  randomStringArrayElement
}
import testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import utest.{Tests, test}

object ScriptHash extends JsTestBase {
  private val scriptHash                     = "scriptHash(callerTestData)"
  private val scriptHashArgBeforeFunc        = "callerTestData.scriptHash()"
  private val invalidScriptHash              = "scriptHash()"
  private val invalidScriptHashArgBeforeFunc = s"callerTestData.scriptHash($randomStringArrayElement)"
  private val testData                       = new TestDataConstantsAndMethods

  val tests: Tests = Tests {
    test.apply("check: function scriptHash for V5 and more compiles for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          scriptHash,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function scriptHash for V5 and more (argument before function) compiles for address") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAddressDataArrayElement,
          scriptHashArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function scriptHash for V5 and more compiles for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          scriptHash,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function scriptHash for V5 and more (argument before function) compiles for alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomAliasDataArrayElement,
          scriptHashArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: scriptHash Non-matching types: expected: Address|Alias") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          scriptHash,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }

    test.apply("compilation error: scriptHash Non-matching types: expected: Address|Alias (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomDigestAlgorithmTypeArrayElement,
          scriptHashArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Address|Alias"))
      }
    }

    test.apply("compilation error: Function 'scriptHash' requires 1 arguments, but 0 are provided") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          invalidScriptHash,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("scriptHash", 1))
      }
    }

    test.apply("compilation error: Function 'scriptHash' requires 1 arguments, but 0 are provided (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomStringArrayElement,
          invalidScriptHashArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("scriptHash", 1))
      }
    }

    test.apply("compilation error: scriptHash function is missing") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomIssuesArrayElement,
          scriptHash,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: scriptHash (argument before function) function is missing") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.codeFromMatchingAndCase(
          randomIssuesArrayElement,
          scriptHashArgBeforeFunc,
          testData.rideV3Result,
          testData.GreaterV3ResultBinaryEntry
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
