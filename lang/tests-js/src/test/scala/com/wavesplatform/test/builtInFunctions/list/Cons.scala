package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object Cons extends JsTestBase {
  // cons
  private val cons                     = "cons(foo, bar)"
  private val consArgBeforeFunc        = "foo.cons(bar)"
  private val invalidCons              = "cons(foo)"
  private val invalidConsArgBeforeFunc = "foo.cons(foo, bar)"
  private val invalidErrorCons  = testData.invalidFunctionError("cons", 2)

  val tests: Tests = Tests {
    test.apply("check: cons function compiles with a stringList") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          stringList,
          cons
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: cons function compiles with a intList") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          cons
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: cons function compiles with a stringList (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomStringArrayElement,
          stringList,
          consArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: cons function compiles with a intList (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          intList,
          consArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: cons - Non-matching types") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          consArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes(""))
      }
    }

    test.apply("compilation error: cons - Non-matching types (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomIssuesArrayElement,
          consArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes(""))
      }
    }

    test.apply("compilation error: Can't find a function overload cons") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidCons
        )
        assertCompileErrorDApp(script, version, invalidErrorCons)
      }
    }

    test.apply("compilation error: Can't find a function overload cons (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script = precondition.simpleRideCode(
          randomInt.toString,
          randomAliasDataArrayElement,
          invalidConsArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, invalidErrorCons)
      }
    }
  }
}
