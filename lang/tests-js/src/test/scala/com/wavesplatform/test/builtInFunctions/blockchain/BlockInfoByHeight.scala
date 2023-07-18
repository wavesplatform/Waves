package com.wavesplatform.test.builtInFunctions.blockchain

import com.wavesplatform.JsTestBase
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomBoolean, randomInt}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object BlockInfoByHeight extends JsTestBase {
  private val blockInfoByHeight              = "blockInfoByHeight(callerTestData)"
  private val blockInfoByHeightArgBeforeFunc = "callerTestData.blockInfoByHeight()"

  private val invalidBlockInfoByHeight = "blockInfoByHeight()"

  val tests: Tests = Tests {
    test("RIDE-31. BlockInfoByHeight function should compile") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        for (
          (intData, function) <- Seq(
            (randomInt.toString, blockInfoByHeight),
            (randomInt.toString, blockInfoByHeightArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(intData, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-32. Negative cases for blockInfoByHeight function when invalid arguments are passed") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BlockInfo", version)
        for (
          (intData, function, error) <- Seq(
            (randomInt.toString, invalidBlockInfoByHeight, invalidFunctionError("blockInfoByHeight", 1)),
            (randomAliasDataArrayElement, blockInfoByHeight, nonMatchingTypes("Int")),
            (randomBoolean.toString, blockInfoByHeightArgBeforeFunc, nonMatchingTypes("Int"))
          )
        ) {
          val script = precondition.onlyMatcherContract(intData, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
