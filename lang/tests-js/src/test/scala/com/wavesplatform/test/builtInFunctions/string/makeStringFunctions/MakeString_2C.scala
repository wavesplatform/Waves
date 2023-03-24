package com.wavesplatform.test.builtInFunctions.string.makeStringFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, nonMatchingTypes, stringList}
import utest.{Tests, test}

object MakeString_2C extends JsTestBase {
  private val makeString_2C                = s"makeString_2C(bar, foo)"
  private val makeString_2CArgBeforeFunc   = s"bar.makeString_2C(foo)"
  private val invalidMakeString_2CFunction = s"makeString_2C(foo)"
  private val invalidErrorMakeString_2C    = testData.invalidFunctionError("makeString_2C", 2)

  val tests: Tests = Tests {
    test("MakeString_2C functions compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function) <- Seq(
          (randomStringArrayElement, stringList, makeString_2C),
          (randomStringArrayElement, stringList, makeString_2CArgBeforeFunc)
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileSuccessDApp(script, V6)
      }
    }

    test("Invalid MakeString_2C functions") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function, error) <- Seq(
          (randomStringArrayElement, stringList, invalidMakeString_2CFunction, invalidErrorMakeString_2C),
          (randomIssuesArrayElement, stringList, makeString_2C, nonMatchingTypes("String")),
          (randomInt.toString, stringList, makeString_2CArgBeforeFunc, nonMatchingTypes("String")),
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileErrorDApp(script, V6, error)
      }
    }

    test("Can't find a function MakeString_2C with versions V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, makeString_2C),
            (randomStringArrayElement, stringList, makeString_2CArgBeforeFunc),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
