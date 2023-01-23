package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement, randomUnionArrayElement}
import utest.{Tests, test}

object Sqrt extends JsTestBase {
  private val union = randomUnionArrayElement

  private val sqrtIntAndUnion              = s"sqrt(callerTestData, $randomInt, $randomInt, $union)"
  private val sqrtIntAndUnionArgBeforeFunc = s"callerTestData.sqrt($randomInt, 3, $union)"
  private val invalidSqrtFunction          = s"sqrt(callerTestData)"

  val tests: Tests = Tests {
    test("check: sqrt Int function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V6)
      val script       = precondition.onlyMatcherContract(randomInt.toString, sqrtIntAndUnion)
      assertCompileSuccessDApp(script, V6)
    }

    test("check: sqrt Int function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V6)
      val script       = precondition.onlyMatcherContract(randomInt.toString, sqrtIntAndUnionArgBeforeFunc)
      assertCompileSuccessDApp(script, V6)
    }

    test("check: sqrt BigInt function compiles") {
      val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", V6)
      val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", sqrtIntAndUnion)
      assertCompileSuccessDApp(script, V6)
    }

    test("check: sqrt BigInt function compiles (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", V6)
      val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", sqrtIntAndUnionArgBeforeFunc)
      assertCompileSuccessDApp(script, V6)
    }

    test("compilation error: invalid sqrt Int function") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V6)
      val script       = precondition.onlyMatcherContract(randomInt.toString, invalidSqrtFunction)
      assertCompileErrorDApp(script, V6, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid sqrt Int data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("Int", V6)
      val script       = precondition.onlyMatcherContract(randomStringArrayElement, sqrtIntAndUnion)
      assertCompileErrorDApp(script, V6, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid sqrt BigInt function") {
      val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", V6)
      val script = precondition.onlyMatcherContract(s"toBigInt(${randomInt.toString})", invalidSqrtFunction)
      assertCompileErrorDApp(script, V6, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: invalid sqrt BigInt data") {
      val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", V6)
      val script = precondition.onlyMatcherContract(randomStringArrayElement, sqrtIntAndUnion)
      assertCompileErrorDApp(script, V6, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
    }

    test("compilation error: sqrt Int and Union Can't find a function for V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, sqrtIntAndUnion)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test("compilation error: sqrt Int and Union Can't find a function for V3 - V5 (argument before function") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, sqrtIntAndUnionArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }
  }
}
