package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersionsWithoutV3, intList, nonMatchingTypes, stringList}
import utest.{Tests, test}

object RemoveByIndex extends JsTestBase {
  private val removeByIndex                     = "removeByIndex(bar, foo)"
  private val removeByIndexArgBeforeFunc        = "bar.removeByIndex(foo)"
  private val invalidRemoveByIndex              = "removeByIndex(foo)"
  private val invalidRemoveByIndexArgBeforeFunc = "foo.removeByIndex(bar, foo)"
  private val invalidErrorRemoveByIndex         = testData.invalidFunctionError("removeByIndex", 2)

  val tests: Tests = Tests {
    test("removeByIndex functions compiles with a list") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomInt.toString, stringList, removeByIndex),
            (randomInt.toString, stringList, removeByIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation errors RemoveByIndex functions") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, removeByIndex, nonMatchingTypes("List[T]")),
            (randomInt.toString, randomIssuesArrayElement, removeByIndexArgBeforeFunc, nonMatchingTypes("List[T]")),
            (randomInt.toString, stringList, invalidRemoveByIndex, invalidErrorRemoveByIndex),
            (randomInt.toString, stringList, invalidRemoveByIndexArgBeforeFunc, invalidErrorRemoveByIndex)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
