package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testData.RandomDataGenerator.{randomAliasDataArrayElement, randomInt}
import testData.{GeneratorContractsForBuiltInFunctions, TestDataConstantsAndMethods}
import utest.{Tests, test}

object BlockInfoByHeight extends JsTestBase {
  private val blockInfoByHeight = "blockInfoByHeight(callerTestData)"
  private val blockInfoByHeightArgBeforeFunc = "callerTestData.blockInfoByHeight()"

  private val invalidBlockInfoByHeight = "blockInfoByHeight()"
  private val invalidBlockInfoByHeightArg = s"$randomAliasDataArrayElement.blockInfoByHeight()"
  private val testData = new TestDataConstantsAndMethods


  val tests: Tests = Tests {
    test.apply("check: function blockInfoByHeight compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          blockInfoByHeight
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function blockInfoByHeight (argument before function) compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          blockInfoByHeightArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: blockInfoByHeight Non-matching type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          blockInfoByHeightArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
      }
    }

    test.apply("compilation error: blockInfoByHeight Non-matching type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          invalidBlockInfoByHeightArg
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("Int"))
      }
    }

    test.apply("compilation error: Function 'blockInfoByHeight' requires 1 arguments") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          invalidBlockInfoByHeight
        )
        assertCompileErrorDApp(script, version, testData.invalidFunctionError("blockInfoByHeight", 1))
      }
    }
  }
}
