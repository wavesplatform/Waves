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
import testHelpers.TestDataConstantsAndMethods.{MATCHING_NOT_EXHAUSTIVE, actualVersions, invalidFunctionError}
import utest.{Tests, test}

object IsDefined extends JsTestBase {
  private val isDefined                     = "isDefined(callerTestData)"
  private val isDefinedArgBeforeFunc        = "callerTestData.isDefined()"
  private val invalidIsDefined              = "isDefined()"
  private val invalidIsDefinedArgBeforeFunc = "callerTestData.isDefined(callerTestData)"
  private val invalidErrorIsDefined         = invalidFunctionError("isDefined", 1)

  val tests: Tests = Tests {
    test("RIDE-228. function isDefined should compile for valid data") {
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

    test("RIDE-229. function isDefined throw a compilation error for can't find overload") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        for (
          (data, function, error) <- Seq(
            (randomUnionArrayElement, invalidIsDefined, MATCHING_NOT_EXHAUSTIVE),
            (randomAddressDataArrayElement, invalidIsDefinedArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE),
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
