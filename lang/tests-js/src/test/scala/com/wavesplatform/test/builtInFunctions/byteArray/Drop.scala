package com.wavesplatform.test.builtInFunctions.byteArray

import com.wavesplatform.JsTestBase
import _root_.testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement}
import _root_.testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result}
import utest.{Tests, test}

object Drop extends JsTestBase {
  private val drop                     = s"drop(callerTestData, $randomInt)"
  private val dropArgBeforeFunction    = s"callerTestData.drop($randomInt)"
  private val invalidDrop              = s"drop(callerTestData)"
  private val invalidDropNotInt        = s"drop(callerTestData, $randomByteVectorArrayElement)"
  private val invalidDropArgBeforeFunc = s"callerTestData.drop(callerTestData, $randomInt)"

  val tests: Tests = Tests {
    test("RIDE-50. Drop function should compile for valid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, drop),
            (randomByteVectorArrayElement, dropArgBeforeFunction)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-51. Drop function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, drop),
            (randomUnionArrayElement, dropArgBeforeFunction),
            (randomByteVectorArrayElement, invalidDrop),
            (randomByteVectorArrayElement, invalidDropNotInt),
            (randomByteVectorArrayElement, invalidDropArgBeforeFunc)
          )
        ) {
          val script = precondition.codeWithoutMatcher(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
    }
  }
}
