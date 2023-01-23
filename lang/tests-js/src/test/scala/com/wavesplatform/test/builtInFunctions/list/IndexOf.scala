package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object IndexOf extends JsTestBase {
  // indexOf
  private val indexOf              = "indexOf(bar, foo)"
  private val indexOfArgBeforeFunc = "bar.indexOf(foo)"
  private val invalidIndexOf       = "indexOf()"

  val tests: Tests = Tests {
    test("check: indexOf function compiles with a stringList") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          stringList,
          indexOf
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: indexOf function compiles with a intList") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          indexOf
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: indexOf function compiles with a stringList (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          stringList,
          indexOfArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: indexOf function compiles with a intList (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          indexOfArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: indexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          stringList,
          stringList,
          indexOf
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: indexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          stringList,
          indexOfArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("compilation error: Can't find a function overload indexOf") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          invalidIndexOf
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
