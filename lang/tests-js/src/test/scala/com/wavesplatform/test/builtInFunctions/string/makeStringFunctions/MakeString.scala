package com.wavesplatform.test.builtInFunctions.string.makeStringFunctions

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.stringList
import utest.{Tests, test}

object MakeString extends JsTestBase {
  // makeString
  private val makeString                     = "makeString(bar, foo)"
  private val makeStringArgBeforeFunc        = "bar.makeString(foo)"
  private val invalidMakeString              = "makeString(foo)"
  private val invalidMakeStringArgBeforeFunc = "foo.makeString(bar, foo)"
  private val invalidErrorMakeString         = testData.invalidFunctionError("makeString", 2)

  val tests: Tests = Tests {
    test("check: makeString function compiles") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: makeString function compiles (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeStringArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test("compilation error: makeString - Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, makeStringArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[String]"))
      }
    }

    test("compilation error: makeString - Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomIssuesArrayElement, makeStringArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("List[String]"))
      }
    }

    test("compilation error: Can't find a function overload makeString") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidMakeString)
        assertCompileErrorDApp(script, version, invalidErrorMakeString)
      }
    }

    test("compilation error: Can't find a function overload makeString (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidMakeStringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidErrorMakeString)
      }
    }
  }

}
