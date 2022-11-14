package com.wavesplatform.test.builtInFunctions.blockchain

import _root_.testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import _root_.testData.RandomDataGenerator.{randomAliasDataArrayElement, randomByteVectorArrayElement, randomInt}
import com.wavesplatform.JsTestBase
import utest.{Tests, test}

object AssetInfo extends JsTestBase {

  private val assetInfo = "assetInfo(callerTestData)"
  private val assetInfoArgBeforeFunc = "callerTestData.assetInfo()"

  private val invalidAssetInfo = "assetInfo()"
  private val invalidAssetInfoArg = s"$randomInt.assetInfo()"
  private val testData = new TestDataConstantsAndMethods


  val tests: Tests = Tests {
    test.apply("check: function assetInfo compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          assetInfo
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function assetInfo (argument before function) compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          assetInfoArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: assetInfo Non-matching type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          assetInfoArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: assetInfo Non-matching type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidAssetInfoArg
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: Function 'assetInfo' requires 1 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Asset", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidAssetInfo
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("assetInfo", 1))
      }
    }
  }
}
