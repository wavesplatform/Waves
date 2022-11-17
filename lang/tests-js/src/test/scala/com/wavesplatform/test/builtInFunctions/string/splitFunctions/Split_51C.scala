package com.wavesplatform.test.builtInFunctions.string.splitFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import utest.{Tests, test}

object Split_51C extends JsTestBase {
  private val split_51C                = s"split_51C(bar, foo)"
  private val split_51CArgBeforeFunc   = s"bar.split_51C(foo)"
  private val invalidSplit_51C = s"split_51C(foo)"
  private val invalidErrorSplit_51C    = testData.invalidFunctionError("split_51C", 2)

  val tests: Tests = Tests {
    test.apply("check: split_51C function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_51C)
      assertCompileSuccessDApp(script, V6)
    }

    test.apply("check: split_51C function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_51CArgBeforeFunc)
      assertCompileSuccessDApp(script, V6)
    }

    test.apply("compilation error: invalid split_51C function") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, invalidSplit_51C)
      assertCompileErrorDApp(script, V6, invalidErrorSplit_51C)
    }

    test.apply("compilation error: invalid split_51C data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomIssuesArrayElement, randomStringArrayElement, split_51C)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test.apply("compilation error: invalid split_51C data (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script = precondition.simpleRideCode(randomInt.toString, randomStringArrayElement, split_51CArgBeforeFunc)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test.apply("compilation error: split_51C Can't find a function for V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_51C)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: split_51C Can't find a function for V3 - V5 (argument before function") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_51CArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
