package com.wavesplatform.test.builtInFunctions.string.makeStringFunctions

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes, stringList}
import utest.{Tests, test}

object MakeString extends JsTestBase {
  private val makeString                     = "makeString(bar, foo)"
  private val makeStringArgBeforeFunc        = "bar.makeString(foo)"
  private val invalidMakeString              = "makeString(foo)"
  private val invalidMakeStringArgBeforeFunc = "foo.makeString(bar, foo)"
  private val invalidErrorMakeString         = invalidFunctionError("makeString", 2)

  val tests: Tests = Tests {
    test("RIDE-192. makeString function should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, makeString),
            (randomStringArrayElement, stringList, makeStringArgBeforeFunc),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-193. makeString function should throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, makeStringArgBeforeFunc, nonMatchingTypes("List[String]")),
            (randomInt.toString, randomIssuesArrayElement, makeStringArgBeforeFunc, nonMatchingTypes("List[String]")),
            (randomStringArrayElement, stringList, invalidMakeString, invalidErrorMakeString),
            (randomStringArrayElement, stringList, invalidMakeStringArgBeforeFunc, invalidErrorMakeString),
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
