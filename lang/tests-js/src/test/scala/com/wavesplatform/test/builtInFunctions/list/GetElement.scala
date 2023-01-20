package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object GetElement extends JsTestBase {
  // getElement
  private val getElement                     = "getElement(bar, foo)"
  private val getElementArgBeforeFunc        = "bar.getElement(foo)"
  private val invalidGetElement              = "getElement(foo)"
  private val invalidGetElementArgBeforeFunc = "foo.getElement(bar, foo)"
  private val invalidErrorGetElement  = testData.invalidFunctionError("getElement", 2)

  val tests: Tests = Tests {
    test.apply("check: getElement function compiles with a stringList") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          stringList,
          getElement
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: getElement function compiles with a intList") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          getElement
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: getElement function compiles with a stringList (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          stringList,
          getElementArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: getElement function compiles with a intList (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          getElementArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: getElement - Non-matching types") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          getElementArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes(""))
      }
    }

    test.apply("compilation error: getElement - Non-matching types (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomIssuesArrayElement,
          getElementArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes(""))
      }
    }

    test.apply("compilation error: Can't find a function overload getElement") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidGetElement
        )
        assertCompileErrorDApp(script, version, invalidErrorGetElement)
      }
    }

    test.apply("compilation error: Can't find a function overload getElement (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidGetElementArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidErrorGetElement)
      }
    }
  }

}
