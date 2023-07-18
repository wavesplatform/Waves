package com.wavesplatform.test.builtInFunctions.math

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V6
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomByteVectorArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, CANT_FIND_FUNCTION}
import utest.{Tests, test}

object Sqrt extends JsTestBase {
  private val union                        = randomUnionArrayElement
  private val sqrtIntAndUnion              = s"sqrt(callerTestData, $randomInt, $randomInt, $union)"
  private val sqrtIntAndUnionArgBeforeFunc = s"callerTestData.sqrt($randomInt, 3, $union)"
  private val invalidSqrtFunction          = s"sqrt(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-189. Sqrt functions compile with Int and BigInt") {
      for (
        (data, function, dataType) <- Seq(
          (randomInt.toString, sqrtIntAndUnion, "Int"),
          (randomInt.toString, sqrtIntAndUnionArgBeforeFunc, "Int"),
          (s"toBigInt(${randomInt.toString})", sqrtIntAndUnion, "BigInt"),
          (s"toBigInt(${randomInt.toString})", sqrtIntAndUnionArgBeforeFunc, "BigInt")
        )
      ) {
        val precondition = new GeneratorContractsForBuiltInFunctions(dataType, V6)
        val script       = precondition.onlyMatcherContract(data, function)
        assertCompileSuccessDApp(script, V6)
      }
    }

    test("RIDE-190. Sqrt functions should throw an error for invalid Int data") {
      for (
        (data, function, dataType) <- Seq(
          (randomStringArrayElement, sqrtIntAndUnion, "Int"),
          (randomInt.toString, invalidSqrtFunction, "Int"),
          (randomByteVectorArrayElement, sqrtIntAndUnion, "BigInt"),
          (s"toBigInt(${randomInt.toString})", invalidSqrtFunction, "BigInt")
        )
      ) {
        val precondition = new GeneratorContractsForBuiltInFunctions(dataType, V6)
        val script       = precondition.onlyMatcherContract(data, function)
        assertCompileErrorDApp(script, V6, CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test("RIDE-191. Can't find a function Sqrt for V3 - V5") {
      for (version <- testData.versionsWithoutV6) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Int", version)
        for (
          (data, function) <- Seq(
            (randomInt.toString, sqrtIntAndUnion),
            (randomInt.toString, sqrtIntAndUnionArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
    }
  }
}
