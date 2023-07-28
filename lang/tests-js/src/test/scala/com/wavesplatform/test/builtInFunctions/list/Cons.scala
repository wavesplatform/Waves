package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomBoolean, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, intList, invalidFunctionError, nonMatchingTypes, stringList}
import utest.{Tests, test}

object Cons extends JsTestBase {
  private val cons                     = "cons(foo, bar)"
  private val consArgBeforeFunc        = "foo.cons(bar)"
  private val invalidCons              = "cons(foo)"
  private val invalidConsArgBeforeFunc = "foo.cons(foo, bar)"
  private val invalidErrorCons         = invalidFunctionError("cons", 2)

  val tests: Tests = Tests {
    test("RIDE-152. Function Cons should compile for valid list") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, stringList, cons),
            (randomInt.toString, intList, cons),
            (randomStringArrayElement, stringList, consArgBeforeFunc),
            (randomInt.toString, intList, consArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-153. Function Cons should throw an error for invalid data or type") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, cons, nonMatchingTypes("")),
            (randomStringArrayElement, randomBoolean.toString, consArgBeforeFunc, nonMatchingTypes("")),
            (randomInt.toString, intList, invalidCons, invalidErrorCons),
            (randomStringArrayElement, stringList, invalidConsArgBeforeFunc, invalidErrorCons)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
