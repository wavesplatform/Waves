package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultStringEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object Drop extends JsTestBase {
  private val drop                     = s"drop(callerTestData, $randomInt)"
  private val dropArgBeforeFunction    = s"callerTestData.drop($randomInt)"
  private val invalidDrop              = s"drop(callerTestData)"
  private val invalidDropArgBeforeFunc = s"callerTestData.drop(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("RIDE-211. function drop should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, drop),
            (randomStringArrayElement, dropArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-212. function drop throw a compilation error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, drop),
            (randomUnionArrayElement, dropArgBeforeFunction),
            (randomStringArrayElement, invalidDrop),
            (randomStringArrayElement, invalidDropArgBeforeFunc)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultStringEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
