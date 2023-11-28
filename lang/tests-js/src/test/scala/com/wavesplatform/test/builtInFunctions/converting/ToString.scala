package com.wavesplatform.test.builtInFunctions.converting

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomBoolean,
  randomDigestAlgorithmTypeArrayElement,
  randomInt,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, versionsSupportingTheNewFeatures}
import utest.{Tests, test}

object ToString extends JsTestBase {
  private val toStr                     = "toString(callerTestData)"
  private val toStrArgBeforeFunc        = "callerTestData.toString()"
  private val invalidToStr              = "toString()"
  private val invalidToStrArgBeforeFunc = "callerTestData.toString(callerTestData)"

  val tests: Tests = Tests {
    test("RIDE-80. Functions toString function should compile with int, string, boolean") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, toStr),
            (randomInt.toString, toStr),
            (randomBoolean.toString, toStr),
            (randomAddressDataArrayElement, toStrArgBeforeFunc),
            (randomInt.toString, toStrArgBeforeFunc),
            (randomBoolean.toString, toStrArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-81. Functions toString should compile with bigInt for V5, V6 versions") {
      for (version <- versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, function) <- Seq(
            (s"toBigInt($randomInt)", toStr),
            (s"toBigInt($randomInt)", toStrArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-82. toString function throws an error for invalid values") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, toStr, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomDigestAlgorithmTypeArrayElement, toStrArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomInt.toString, invalidToStr, CANT_FIND_A_FUNCTION_OVERLOAD),
            (randomAddressDataArrayElement, invalidToStrArgBeforeFunc, CANT_FIND_A_FUNCTION_OVERLOAD)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
