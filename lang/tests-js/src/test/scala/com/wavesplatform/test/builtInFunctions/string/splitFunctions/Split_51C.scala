package com.wavesplatform.test.builtInFunctions.string.splitFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, nonMatchingTypes}
import utest.{Tests, test}

object Split_51C extends JsTestBase {
  private val split_51C              = s"split_51C(bar, foo)"
  private val split_51CArgBeforeFunc = s"bar.split_51C(foo)"
  private val invalidSplit_51C       = s"split_51C(foo)"
  private val invalidErrorSplit_51C  = testData.invalidFunctionError("split_51C", 2)

  val tests: Tests = Tests {
    test("RIDE-205. split_51C function should compile for valid data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function) <- Seq(
          (randomStringArrayElement, randomStringArrayElement, split_51C),
          (randomStringArrayElement, randomStringArrayElement, split_51CArgBeforeFunc)
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileSuccessDApp(script, V6)
      }
    }

    test("RIDE-206. split_51C function should throw a compilation error for invalid data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function, error) <- Seq(
          (randomIssuesArrayElement, randomStringArrayElement, split_51C, nonMatchingTypes("String")),
          (randomInt.toString, randomStringArrayElement, split_51CArgBeforeFunc, nonMatchingTypes("String")),
          (randomStringArrayElement, randomStringArrayElement, invalidSplit_51C, invalidErrorSplit_51C)
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileErrorDApp(script, V6, error)
      }
    }

    test("RIDE-207. Can't find a function split_4C for RIDE versions V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, randomStringArrayElement, split_51C),
            (randomStringArrayElement, randomStringArrayElement, split_51CArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
