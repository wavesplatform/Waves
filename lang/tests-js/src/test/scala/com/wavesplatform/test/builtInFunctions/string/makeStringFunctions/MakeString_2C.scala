package com.wavesplatform.test.builtInFunctions.string.makeStringFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.stringList
import utest.{Tests, test}

object MakeString_2C extends JsTestBase {
  private val makeString_2C                = s"makeString_2C(bar, foo)"
  private val makeString_2CArgBeforeFunc   = s"bar.makeString_2C(foo)"
  private val invalidMakeString_2CFunction = s"makeString_2C(foo)"
  private val invalidErrorMakeString_2C    = testData.invalidFunctionError("makeString_2C", 2)

  val tests: Tests = Tests {
    test.apply("check: makeString_2C function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_2C)
      assertCompileSuccessDApp(script, V6)
    }

    test.apply("check: makeString_2C function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_2CArgBeforeFunc)
      assertCompileSuccessDApp(script, V6)
    }

    test.apply("compilation error: invalid makeString_2C function") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, invalidMakeString_2CFunction)
      assertCompileErrorDApp(script, V6, invalidErrorMakeString_2C)
    }

    test.apply("compilation error: invalid makeString_2C data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script       = precondition.simpleRideCode(randomIssuesArrayElement, stringList, makeString_2C)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test.apply("compilation error: invalid makeString_2C data (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V6)
      val script = precondition.simpleRideCode(randomInt.toString, stringList, makeString_2CArgBeforeFunc)
      assertCompileErrorDApp(script, V6, testData.nonMatchingTypes("String"))
    }

    test.apply("compilation error: makeString_2C Can't find a function for V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_2C)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test.apply("compilation error: makeString_2C Can't find a function for V3 - V5 (argument before function") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, makeString_2CArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
