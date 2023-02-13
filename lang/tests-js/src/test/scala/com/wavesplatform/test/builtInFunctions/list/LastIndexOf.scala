package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomByteVectorArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, intList, stringList}
import utest.{Tests, test}

object LastIndexOf extends JsTestBase {
  private val lastIndexOf                     = "lastIndexOf(bar, foo)"
  private val lastIndexOfArgBeforeFunc        = "bar.lastIndexOf(foo)"
  private val invalidLastIndexOf              = "lastIndexOf()"
  private val invalidLastIndexOfArgBeforeFunc = "bar.indexOf(bar, foo)"

  val tests: Tests = Tests {
    test("LastIndexOf functions compiles with a list") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, lastIndexOf),
            (randomInt.toString, intList, lastIndexOf),
            (randomStringArrayElement, stringList, lastIndexOfArgBeforeFunc),
            (randomInt.toString, intList, lastIndexOfArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation errors LastIndexOf functions") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (stringList, stringList, lastIndexOf),
            (randomByteVectorArrayElement, intList, lastIndexOfArgBeforeFunc),
            (randomInt.toString, intList, invalidLastIndexOf),
            (randomStringArrayElement, stringList, invalidLastIndexOfArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
