package com.wavesplatform.test.builtInFunctions.string.splitFunctions

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, nonMatchingTypes}
import utest.{Tests, test}

object Split extends JsTestBase {
  private val split                     = "split(bar, foo)"
  private val splitArgBeforeFunc        = "bar.split(foo)"
  private val invalidSplit              = "split(foo)"
  private val invalidSplitArgBeforeFunc = "foo.split(bar, foo)"
  private val invalidErrorSplit         = testData.invalidFunctionError("split", 2)

  val tests: Tests = Tests {
    test("RIDE-200. split function should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, randomStringArrayElement, split),
            (randomStringArrayElement, randomStringArrayElement, splitArgBeforeFunc),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-201. split function should throw a compilation error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, splitArgBeforeFunc, nonMatchingTypes("String")),
            (randomInt.toString, randomIssuesArrayElement, splitArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, randomStringArrayElement, invalidSplit, invalidErrorSplit),
            (randomStringArrayElement, randomStringArrayElement, invalidSplitArgBeforeFunc, invalidErrorSplit),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
