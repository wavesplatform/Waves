package com.wavesplatform.test.builtInFunctions.string.splitFunctions

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import utest.{Tests, test}

object Split extends JsTestBase {
  // split
  private val split                     = "split(bar, foo)"
  private val splitArgBeforeFunc        = "bar.split(foo)"
  private val invalidSplit              = "split(foo)"
  private val invalidSplitArgBeforeFunc = "foo.split(bar, foo)"
  private val invalidErrorSplit         = testData.invalidFunctionError("split", 2)

  val tests: Tests = Tests {
    test("check: split function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: split function compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, splitArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: split - Non-matching types") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, splitArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: split - Non-matching types (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomIssuesArrayElement, splitArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: Can't find a function overload split") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidSplit)
        assertCompileErrorDApp(script, version, invalidErrorSplit)
      }
    }

    test("compilation error: Can't find a function overload split (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidSplitArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidErrorSplit)
      }
    }
  }

}
