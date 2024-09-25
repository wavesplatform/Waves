package com.wavesplatform.test.builtInFunctions.blockchain

import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import _root_.testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomInt}
import com.wavesplatform.JsTestBase
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object AssetInfo extends JsTestBase {

  private val assetInfo              = "assetInfo(callerTestData)"
  private val assetInfoArgBeforeFunc = "callerTestData.assetInfo()"

  private val invalidAssetInfo    = "assetInfo()"
  private val invalidAssetInfoArg = s"$randomInt.assetInfo()"

  val tests: Tests = Tests {
    test("RIDE-30. Compile assetInfo function for asset") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        for (
          (asset, function) <- Seq(
            (randomByteVectorArrayElement, assetInfo),
            (randomByteVectorArrayElement, assetInfoArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(asset, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-29. Invalid data must be validated") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        for (
          (asset, function, error) <- Seq(
            (randomByteVectorArrayElement, invalidAssetInfo, invalidFunctionError("assetInfo", 1)),
            (randomByteVectorArrayElement, invalidAssetInfoArg, nonMatchingTypes("ByteVector")),
            (randomAliasDataArrayElement, assetInfo, nonMatchingTypes("ByteVector")),
            (randomBoolean.toString, assetInfoArgBeforeFunc, nonMatchingTypes("ByteVector"))
          )
        ) {
          val script = precondition.onlyMatcherContract(asset, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
