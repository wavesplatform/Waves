package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object RemoveByIndex extends JsTestBase {
  // removeByIndex
  private val removeByIndex                     = "removeByIndex(bar, foo)"
  private val removeByIndexArgBeforeFunc        = "bar.removeByIndex(foo)"
  private val invalidRemoveByIndex              = "removeByIndex(foo)"
  private val invalidRemoveByIndexArgBeforeFunc = "foo.removeByIndex(bar, foo)"
  private val invalidErrorRemoveByIndex         = testData.invalidFunctionError("removeByIndex", 2)

  val tests: Tests = Tests {
    test.apply("check: removeByIndex function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, stringList, removeByIndex)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: removeByIndex function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, stringList, removeByIndexArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: removeByIndex - Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, removeByIndexArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[T]"))
      }
    }

    test.apply("compilation error: removeByIndex - Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomIssuesArrayElement, removeByIndexArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[T]"))
      }
    }

    test.apply("compilation error: Can't find a function overload removeByIndex") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidRemoveByIndex)
        assertCompileErrorDApp(script, version, invalidErrorRemoveByIndex)
      }
    }

    test.apply("compilation error: Can't find a function overload removeByIndex (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidRemoveByIndexArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidErrorRemoveByIndex)
      }
    }
  }

}
