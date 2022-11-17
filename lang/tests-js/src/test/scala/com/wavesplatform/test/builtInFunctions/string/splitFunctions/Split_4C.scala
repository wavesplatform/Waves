package com.wavesplatform.test.builtInFunctions.string.splitFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import utest.{Tests, test}

object Split_4C extends JsTestBase {
  private val split_4C                = s"split_4C(bar, foo)"
  private val split_4CArgBeforeFunc   = s"bar.split_4C(foo)"
  private val invalidSplit_4C = s"split_4C(foo)"
  private val invalidErrorSplit_4C    = testData.invalidFunctionError("split_4C", 2)

  val tests: Tests = Tests {
    test.apply("check: split_4C function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_4C)
      assertCompileSuccessDApp(script, V6)
    }

    test.apply("check: split_4C function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_4CArgBeforeFunc)
      assertCompileSuccessDApp(script, V6)
    }

    test.apply("compilation error: invalid split_4C function") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, invalidSplit_4C)
      assertCompileErrorDApp(script, V6, invalidErrorSplit_4C)
    }

    test.apply("compilation error: invalid split_4C data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomIssuesArrayElement, randomStringArrayElement, split_4C)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test.apply("compilation error: invalid split_4C data (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script = precondition.simpleRideCode(randomInt.toString, randomStringArrayElement, split_4CArgBeforeFunc)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test.apply("compilation error: split_4C Can't find a function for V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_4C)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: split_4C Can't find a function for V3 - V5 (argument before function") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, split_4CArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
