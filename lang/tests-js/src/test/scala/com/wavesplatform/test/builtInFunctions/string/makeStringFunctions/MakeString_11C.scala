package com.wavesplatform.test.builtInFunctions.string.makeStringFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, nonMatchingTypes, stringList}
import utest.{Tests, test}

object MakeString_11C extends JsTestBase {
  private val makeString_11C                = s"makeString_11C(bar, foo)"
  private val makeString_11CArgBeforeFunc   = s"bar.makeString_11C(foo)"
  private val invalidMakeString_11CFunction = s"makeString_11C(foo)"
  private val invalidErrorMakeString_11C    = testData.invalidFunctionError("makeString_11C", 2)

  val tests: Tests = Tests {
    test("MakeString_11C functions compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function) <- Seq(
          (randomStringArrayElement, stringList, makeString_11C),
          (randomStringArrayElement, stringList, makeString_11CArgBeforeFunc)
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileSuccessDApp(script, V6)
      }
    }

    test("Invalid MakeString_11C functions") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      for (
        (data, list, function, error) <- Seq(
          (randomStringArrayElement, stringList, invalidMakeString_11CFunction, invalidErrorMakeString_11C),
          (randomIssuesArrayElement, stringList, makeString_11C, nonMatchingTypes("String")),
          (randomInt.toString, stringList, makeString_11CArgBeforeFunc, nonMatchingTypes("String")),
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileErrorDApp(script, V6, error)
      }
    }

    test("Can't find a function MakeString_11C with versions V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, makeString_11C),
            (randomStringArrayElement, stringList, makeString_11CArgBeforeFunc),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
