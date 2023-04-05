package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomStringArrayElement
}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_FUNCTION,
  GreaterV3ResultBinaryEntry,
  invalidFunctionError,
  nonMatchingTypes,
  oldVersions,
  rideV3Result,
  thisVariable,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object ScriptHash extends JsTestBase {
  private val scriptHash                     = "scriptHash(callerTestData)"
  private val scriptHashArgBeforeFunc        = "callerTestData.scriptHash()"
  private val invalidScriptHash              = "scriptHash(callerTestData, callerTestData)"
  private val invalidScriptHashArgBeforeFunc = s"callerTestData.scriptHash($randomStringArrayElement)"

  val tests: Tests = Tests {
    test("Functions ScriptHash for V5 and more compiles for address") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, scriptHash),
            (randomAddressDataArrayElement, scriptHashArgBeforeFunc),
            (randomAliasDataArrayElement, scriptHash),
            (randomAliasDataArrayElement, scriptHashArgBeforeFunc),
            (thisVariable, scriptHash),
            (thisVariable, scriptHashArgBeforeFunc)
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("negative cases ScriptHash for V5 and more") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomStringArrayElement, scriptHash, nonMatchingTypes("Address|Alias")),
            (randomDigestAlgorithmTypeArrayElement, scriptHashArgBeforeFunc, nonMatchingTypes("Address|Alias")),
            (randomAddressDataArrayElement, invalidScriptHash, invalidFunctionError("scriptHash", 1)),
            (randomAliasDataArrayElement, invalidScriptHash, invalidFunctionError("scriptHash", 1)),
            (thisVariable, invalidScriptHashArgBeforeFunc, invalidFunctionError("scriptHash", 1))
          )
        ) {
          val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("negative cases CalculateAssetId for version V3, V4") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, scriptHash),
            (randomAliasDataArrayElement, scriptHashArgBeforeFunc)
          )
        ) {
          val script = precondition.codeForCalculateLeaseId(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
