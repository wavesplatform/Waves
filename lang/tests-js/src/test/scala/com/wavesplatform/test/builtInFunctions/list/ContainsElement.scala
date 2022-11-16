package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object ContainsElement extends JsTestBase {
  // containsElement
  private val containsElement                     = "containsElement(bar, foo)"
  private val containsElementArgBeforeFunc        = "bar.containsElement(foo)"
  private val invalidContainsElement              = "containsElement(foo)"
  private val invalidContainsElementArgBeforeFunc = "foo.containsElement(foo, bar)"
  private val invalidErrorContainsElement         = testData.invalidFunctionError("containsElement", 2)

  val tests: Tests = Tests {
    test.apply("check: containsElement function compiles with a stringList") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomStringArrayElement, stringList, containsElement)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: containsElement function compiles with a intList") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomInt.toString, intList, containsElement)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: containsElement function compiles with a stringList (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomStringArrayElement, stringList, containsElementArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: containsElement function compiles with a intList (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomInt.toString, intList, containsElementArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: containsElement - Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, containsElementArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes(""))
      }
    }

    test.apply("compilation error: containsElement - Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomInt.toString, randomIssuesArrayElement, containsElementArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes(""))
      }
    }

    test.apply("compilation error: Can't find a function overload containsElement") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidContainsElement)
        assertCompileErrorDApp(script, version, invalidErrorContainsElement)
      }
    }

    test.apply("compilation error: Can't find a function overload containsElement (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidContainsElementArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorContainsElement)
      }
    }
  }
}
