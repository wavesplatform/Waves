package com.wavesplatform.test.builtInFunctions.string.splitFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, nonMatchingTypes}
import utest.{Tests, test}

object Split_4C extends JsTestBase {
  private val split_4C              = s"split_4C(bar, foo)"
  private val split_4CArgBeforeFunc = s"bar.split_4C(foo)"
  private val invalidSplit_4C       = s"split_4C(foo)"
  private val invalidErrorSplit_4C  = testData.invalidFunctionError("split_4C", 2)

  val tests: Tests = Tests {
    test("split_4C functions compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function) <- Seq(
          (randomStringArrayElement, randomStringArrayElement, split_4C),
          (randomStringArrayElement, randomStringArrayElement, split_4CArgBeforeFunc)
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileSuccessDApp(script, V6)
      }
    }

    test("compilation error split_4C functions") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function, error) <- Seq(
          (randomIssuesArrayElement, randomStringArrayElement, split_4C, nonMatchingTypes("String")),
          (randomInt.toString, randomStringArrayElement, split_4CArgBeforeFunc, nonMatchingTypes("String")),
          (randomStringArrayElement, randomStringArrayElement, invalidSplit_4C, invalidErrorSplit_4C),
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileErrorDApp(script, V6, error)
      }
    }

    test("Can't find a function split_4C with versions V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, randomStringArrayElement, split_4C),
            (randomStringArrayElement, randomStringArrayElement, split_4CArgBeforeFunc),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
