package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomInt,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError}
import utest.{Tests, test}

object IsDefined extends JsTestBase {
  private val isDefined                     = "isDefined(callerTestData)"
  private val isDefinedArgBeforeFunc        = "callerTestData.isDefined()"
  private val invalidIsDefined              = "isDefined()"
  private val invalidIsDefinedArgBeforeFunc = "callerTestData.isDefined(callerTestData)"
  private val invalidErrorIsDefined         = invalidFunctionError("isDefined", 1)

  val tests: Tests = Tests {
    test("IsDefined functions compiles") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function) <- Seq(
            (randomAddressDataArrayElement, isDefined),
            (randomAliasDataArrayElement, isDefinedArgBeforeFunc),
            (randomByteVectorArrayElement, isDefined),
            (randomStringArrayElement, isDefinedArgBeforeFunc),
            (randomUnionArrayElement, isDefined),
            (randomDigestAlgorithmTypeArrayElement, isDefinedArgBeforeFunc),
            (randomInt.toString, isDefined),
            (randomBoolean.toString, isDefinedArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("Can't find a function overload isDefined") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, invalidIsDefined, invalidErrorIsDefined),
            (randomInt.toString, invalidIsDefinedArgBeforeFunc, invalidErrorIsDefined)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
