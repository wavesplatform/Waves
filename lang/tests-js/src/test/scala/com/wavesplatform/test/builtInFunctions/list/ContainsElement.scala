package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersionsWithoutV3, intList, nonMatchingTypes, stringList}
import utest.{Tests, test}

object ContainsElement extends JsTestBase {
  // containsElement
  private val containsElement                     = "containsElement(bar, foo)"
  private val containsElementArgBeforeFunc        = "bar.containsElement(foo)"
  private val invalidContainsElement              = "containsElement(foo)"
  private val invalidContainsElementArgBeforeFunc = "foo.containsElement(foo, bar)"
  private val invalidErrorContainsElement         = testData.invalidFunctionError("containsElement", 2)

  val tests: Tests = Tests {
    test("ContainsElement functions compiles with a list") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, containsElement),
            (randomInt.toString, intList, containsElement),
            (randomStringArrayElement, stringList, containsElementArgBeforeFunc),
            (randomInt.toString, intList, containsElementArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation errors Cons functions") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, containsElement, nonMatchingTypes("")),
            (randomStringArrayElement, randomIssuesArrayElement, containsElementArgBeforeFunc, nonMatchingTypes("")),
            (randomInt.toString, intList, invalidContainsElement, invalidErrorContainsElement),
            (randomStringArrayElement, stringList, invalidContainsElementArgBeforeFunc, invalidErrorContainsElement)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
