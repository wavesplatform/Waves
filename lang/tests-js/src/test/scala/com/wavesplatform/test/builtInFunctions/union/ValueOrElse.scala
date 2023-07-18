package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomInt,
  randomStringArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_FUNCTION, CANT_MATCH_INFERRED_TYPE, actualVersionsWithoutV3, invalidFunctionError}
import utest.{Tests, test}

object ValueOrElse extends JsTestBase {
  private val valueOrElse                     = "valueOrElse(bar, foo)"
  private val valueOrElseArgBeforeFunc        = "bar.valueOrElse(foo)"
  private val invalidValueOrElse              = "valueOrElse(foo)"
  private val invalidValueOrElseArgBeforeFunc = "foo.valueOrElse(foo, bar)"
  private val invalidErrorValueOrElse         = invalidFunctionError("valueOrElse", 2)

  val tests: Tests = Tests {
    test("RIDE-232. function valueOrElse should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (firstData, secondData, function) <- Seq(
            (randomStringArrayElement, randomStringArrayElement, valueOrElse),
            (randomInt.toString, randomInt.toString, valueOrElse),
            (randomAliasDataArrayElement, randomAliasDataArrayElement, valueOrElse),
            (randomAddressDataArrayElement, randomAddressDataArrayElement, valueOrElseArgBeforeFunc),
            (randomByteVectorArrayElement, randomByteVectorArrayElement, valueOrElseArgBeforeFunc),
            (randomBoolean.toString, randomBoolean.toString, valueOrElseArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(firstData, secondData, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-233. function valueOrElse throw a compilation error for invalid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (firstData, secondData, function, error) <- Seq(
            (randomInt.toString, randomByteVectorArrayElement, valueOrElse, CANT_MATCH_INFERRED_TYPE),
            (randomInt.toString, randomStringArrayElement, valueOrElseArgBeforeFunc, CANT_MATCH_INFERRED_TYPE),
            (randomStringArrayElement, randomStringArrayElement, invalidValueOrElse, invalidErrorValueOrElse),
            (randomByteVectorArrayElement, randomByteVectorArrayElement, invalidValueOrElseArgBeforeFunc, invalidErrorValueOrElse)
          )
        ) {
          val script = precondition.simpleRideCode(firstData, secondData, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("RIDE-234. Can't find a function valueOrElse for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      for (
        (firstData, secondData, function) <- Seq(
          (randomAliasDataArrayElement, randomAliasDataArrayElement, valueOrElse),
          (randomAddressDataArrayElement, randomAddressDataArrayElement, valueOrElseArgBeforeFunc)
        )
      ) {
        val script = precondition.simpleRideCode(firstData, secondData, function)
        assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
    }
  }
}
