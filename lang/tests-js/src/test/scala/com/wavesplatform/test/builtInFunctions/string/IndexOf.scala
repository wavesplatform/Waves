package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, intList, stringList}
import utest.{Tests, test}

object IndexOf extends JsTestBase {
  private val indexOf                        = "indexOf(bar, foo)"
  private val indexOfWithOffset              = s"indexOf(bar, foo, $randomInt)"
  private val indexOfArgBeforeFunc           = "bar.indexOf(foo)"
  private val indexOfWithOffsetArgBeforeFunc = s"bar.indexOf(foo, $randomInt)"
  private val invalidIndexOf                 = "indexOf()"

  val tests: Tests = Tests {
    test("check: indexOf function compiles") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (firstData, secondData, function) <- Seq(
            (randomStringArrayElement, randomStringArrayElement, indexOf),
            (randomStringArrayElement, randomStringArrayElement, indexOfWithOffset),
            (randomStringArrayElement, randomStringArrayElement, indexOfArgBeforeFunc),
            (randomStringArrayElement, randomStringArrayElement, indexOfWithOffsetArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(firstData, secondData, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("compilation error: indexOf - Non-matching types - Can't find a function overload") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (firstData, secondData, function) <- Seq(
            (stringList, randomStringArrayElement, indexOf),
            (randomInt.toString, randomStringArrayElement, indexOfArgBeforeFunc),
            (stringList, randomStringArrayElement, indexOfWithOffset),
            (randomInt.toString, randomStringArrayElement, indexOfWithOffsetArgBeforeFunc),
            (randomInt.toString, intList, invalidIndexOf)
          )
        ) {
          val script = precondition.simpleRideCode(firstData, secondData, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
