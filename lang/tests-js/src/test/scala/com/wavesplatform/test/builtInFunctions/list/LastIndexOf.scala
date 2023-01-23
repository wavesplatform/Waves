package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object LastIndexOf extends JsTestBase {
  // lastIndexOf
  private val lastIndexOf              = "lastIndexOf(bar, foo)"
  private val lastIndexOfArgBeforeFunc = "bar.lastIndexOf(foo)"
  private val invalidLastIndexOf       = "lastIndexOf()"

  val tests: Tests = Tests {
    test("check: lastIndexOf function compiles with a stringList") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          stringList,
          lastIndexOf
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: lastIndexOf function compiles with a intList") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          lastIndexOf
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: lastIndexOf function compiles with a stringList (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          stringList,
          lastIndexOfArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: lastIndexOf function compiles with a intList (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          lastIndexOfArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: lastIndexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          stringList,
          stringList,
          lastIndexOf
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: lastIndexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          stringList,
          lastIndexOfArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload lastIndexOf") {
      for (version <- testData.actualVersionsWithoutV3) {
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
