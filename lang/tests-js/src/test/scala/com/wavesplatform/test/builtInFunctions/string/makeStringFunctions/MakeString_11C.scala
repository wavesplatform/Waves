package com.wavesplatform.test.builtInFunctions.string.makeStringFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.stringList
import utest.{Tests, test}

object MakeString_11C extends JsTestBase {
  private val makeString_11C                = s"makeString_11C(bar, foo)"
  private val makeString_11CArgBeforeFunc   = s"bar.makeString_11C(foo)"
  private val invalidMakeString_11CFunction = s"makeString_11C(foo)"
  private val invalidErrorMakeString_11C    = testData.invalidFunctionError("makeString_11C", 2)

  val tests: Tests = Tests {
    test("check: makeString_11C function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_11C)
      assertCompileSuccessDApp(script, V6)
    }

    test("check: makeString_11C function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_11CArgBeforeFunc)
      assertCompileSuccessDApp(script, V6)
    }

    test("compilation error: invalid makeString_11C function") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, invalidMakeString_11CFunction)
      assertCompileErrorDApp(script, V6, invalidErrorMakeString_11C)
    }

    test("compilation error: invalid makeString_11C data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomIssuesArrayElement, stringList, makeString_11C)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test("compilation error: invalid makeString_11C data (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script = precondition.simpleRideCode(randomInt.toString, stringList, makeString_11CArgBeforeFunc)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test("compilation error: makeString_11C Can't find a function for V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_11C)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test("compilation error: makeString_11C Can't find a function for V3 - V5 (argument before function") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_11CArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
