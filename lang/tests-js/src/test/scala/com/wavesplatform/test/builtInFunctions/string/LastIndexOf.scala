package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object LastIndexOf extends JsTestBase {
  // lastIndexOf
  private val lastIndexOf                        = "lastIndexOf(bar, foo)"
  private val lastIndexOfWithOffset              = s"lastIndexOf(bar, foo, $randomInt)"
  private val lastIndexOfArgBeforeFunc           = "bar.lastIndexOf(foo)"
  private val lastIndexOfWithOffsetArgBeforeFunc = s"bar.lastIndexOf(foo, $randomInt)"
  private val invalidLastIndexOf                 = "lastIndexOf()"

  val tests: Tests = Tests {
    test("check: lastIndexOf function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          lastIndexOf
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: lastIndexOf function compiles with offset") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          lastIndexOfWithOffset
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: lastIndexOf function compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          lastIndexOfArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: lastIndexOf function compiles with offset (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          lastIndexOfWithOffsetArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: lastIndexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          stringList,
          randomStringArrayElement,
          lastIndexOf
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: lastIndexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomStringArrayElement,
          lastIndexOfArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: lastIndexOf with offset - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          stringList,
          randomStringArrayElement,
          lastIndexOfWithOffset
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: lastIndexOf with overload - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomStringArrayElement,
          lastIndexOfWithOffsetArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload lastIndexOf") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          invalidLastIndexOf
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }

}
