package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import utest.{Tests, test}

object Contains extends JsTestBase {
  // contains
  private val contains                     = "contains(bar, foo)"
  private val containsArgBeforeFunc        = "bar.contains(foo)"
  private val invalidContains              = "contains(foo)"
  private val invalidContainsArgBeforeFunc = "foo.contains(bar, foo)"
  private val invalidErrorContains         = testData.invalidFunctionError("contains", 2)

  val tests: Tests = Tests {
    test("check: contains function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, contains)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: contains function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, containsArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: contains - Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, containsArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: contains - Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomIssuesArrayElement, containsArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: Can't find a function overload contains") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidContains)
        assertCompileErrorDApp(script, version, invalidErrorContains)
      }
    }

    test("compilation error: Can't find a function overload contains (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidContainsArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidErrorContains)
      }
    }

    test("compilation error: Can't find a function for V3") {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
        val script = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, contains)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
