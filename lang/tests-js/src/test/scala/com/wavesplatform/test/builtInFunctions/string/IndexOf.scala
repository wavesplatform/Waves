package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object IndexOf extends JsTestBase {
  // indexOf
  private val indexOf                        = "indexOf(bar, foo)"
  private val indexOfWithOffset              = s"indexOf(bar, foo, $randomInt)"
  private val indexOfArgBeforeFunc           = "bar.indexOf(foo)"
  private val indexOfWithOffsetArgBeforeFunc = s"bar.indexOf(foo, $randomInt)"
  private val invalidIndexOf                 = "indexOf()"

  val tests: Tests = Tests {
    test.apply("check: indexOf function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          indexOf
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: indexOf function compiles with offset") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          indexOfWithOffset
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: indexOf function compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          indexOfArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: indexOf function compiles with offset (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          randomStringArrayElement,
          indexOfWithOffsetArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: indexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          stringList,
          randomStringArrayElement,
          indexOf
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: indexOf - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomStringArrayElement,
          indexOfArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: indexOf with offset - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          stringList,
          randomStringArrayElement,
          indexOfWithOffset
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: indexOf with overload - Non-matching types - Can't find a function overload") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomStringArrayElement,
          indexOfWithOffsetArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload indexOf") {
      for (version <- testData.actualVersions) {
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
