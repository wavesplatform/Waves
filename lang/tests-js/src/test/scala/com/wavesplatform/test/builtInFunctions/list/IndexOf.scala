package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomByteVectorArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, intList, stringList}
import utest.{Tests, test}

object IndexOf extends JsTestBase {
  private val indexOf                     = "indexOf(bar, foo)"
  private val indexOfArgBeforeFunc        = "bar.indexOf(foo)"
  private val invalidIndexOf              = "indexOf()"
  private val invalidIndexOfArgBeforeFunc = "bar.indexOf(bar, foo)"

  val tests: Tests = Tests {
    test("RIDE-158. Function IndexOf should compile for valid list") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, indexOf),
            (randomInt.toString, intList, indexOf),
            (randomStringArrayElement, stringList, indexOfArgBeforeFunc),
            (randomInt.toString, intList, indexOfArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-159. Function IndexOf should throw an error for invalid data or type") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (stringList, stringList, indexOf),
            (randomByteVectorArrayElement, intList, indexOfArgBeforeFunc),
            (randomInt.toString, intList, invalidIndexOf),
            (randomStringArrayElement, stringList, invalidIndexOfArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
