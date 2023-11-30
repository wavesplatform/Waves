package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V8}
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{nonMatchingTypes, stringList}
import utest.{Tests, test}

object ReplaceByIndex extends JsTestBase {
  private val replaceByIndex                     = "replaceByIndex(bar, 1, foo)"
  private val replaceByIndexArgBeforeFunc        = "bar.replaceByIndex(1, foo)"
  private val invalidReplaceByIndex              = "replaceByIndex(1, foo)"
  private val invalidReplaceByIndexArgBeforeFunc = "foo.replaceByIndex(bar, 1, foo)"
  private val invalidErrorReplaceByIndex         = testData.invalidFunctionError("replaceByIndex", numberOfArguments = 3)

  val tests: Tests = Tests {
    test("replaceByIndex functions compiles with a list") {
      for (version <- DirectiveDictionary[StdLibVersion].all.filter(_ >= V8)) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, replaceByIndex),
            (randomStringArrayElement, stringList, replaceByIndexArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Compilation errors ReplaceByIndex functions") {
      for (version <- DirectiveDictionary[StdLibVersion].all.filter(_ >= V8)) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, replaceByIndex, nonMatchingTypes("List[T]")),
            (randomInt.toString, randomIssuesArrayElement, replaceByIndexArgBeforeFunc, nonMatchingTypes("List[T]")),
            (randomInt.toString, stringList, invalidReplaceByIndex, invalidErrorReplaceByIndex),
            (randomInt.toString, stringList, invalidReplaceByIndexArgBeforeFunc, invalidErrorReplaceByIndex)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
