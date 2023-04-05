package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, intList, nonMatchingTypes, stringList}
import utest.{Tests, test}

object GetElement extends JsTestBase {
  private val getElement                     = "getElement(bar, foo)"
  private val getElementArgBeforeFunc        = "bar.getElement(foo)"
  private val invalidGetElement              = "getElement(foo)"
  private val invalidGetElementArgBeforeFunc = "foo.getElement(bar, foo)"
  private val invalidErrorGetElement         = testData.invalidFunctionError("getElement", 2)

  val tests: Tests = Tests {
    test("GetElement functions compiles with a list") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomInt.toString, stringList, getElement),
            (randomInt.toString, intList, getElement),
            (randomInt.toString, stringList, getElementArgBeforeFunc),
            (randomInt.toString, intList, getElementArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation errors GetElement functions") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, getElement, nonMatchingTypes("")),
            (randomStringArrayElement, randomIssuesArrayElement, getElementArgBeforeFunc, nonMatchingTypes("")),
            (randomInt.toString, intList, invalidGetElement, invalidErrorGetElement),
            (randomStringArrayElement, stringList, invalidGetElementArgBeforeFunc, invalidErrorGetElement)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
