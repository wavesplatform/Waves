package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomInt,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{MATCHING_NOT_EXHAUSTIVE, actualVersions}
import utest.{Tests, test}

object ValueOrErrorMessage extends JsTestBase {
  private val valueOrErrorMessage                     = s"valueOrErrorMessage(callerTestData, \"error message\")"
  private val valueOrErrorMessageArgBeforeFunction    = s"callerTestData.valueOrErrorMessage(\"error message\")"
  private val invalidValueOrErrorMessage              = s"valueOrErrorMessage(callerTestData)"
  private val invalidValueOrErrorMessageArgBeforeFunc = s"callerTestData.valueOrErrorMessage(callerTestData, \"error message\")"
  private val valueOrErrorInvalidFunctionMessage      = testData.invalidFunctionError("valueOrErrorMessage", 2)

  val tests: Tests = Tests {
    test("RIDE-235. valueOrErrorMessage functions are compiled with valid data types.") {
      for (version <- actualVersions) {
        for (
          (dataType, firstData, function) <- Seq(
            ("Int", randomInt.toString, valueOrErrorMessage),
            ("Boolean", randomBoolean.toString, valueOrErrorMessage),
            ("String", randomStringArrayElement, valueOrErrorMessage),
            ("ByteVector", randomByteVectorArrayElement, valueOrErrorMessageArgBeforeFunction),
            ("Address", randomAddressDataArrayElement, valueOrErrorMessageArgBeforeFunction),
            ("Alias", randomAliasDataArrayElement, valueOrErrorMessageArgBeforeFunction)
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(firstData, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-236. valueOrErrorMessage function should throw a compilation error for invalid data.") {
      for (version <- actualVersions) {
        for (
          (dataType, firstData, function, error) <- Seq(
            ("Alias", randomAddressDataArrayElement, valueOrErrorMessage, MATCHING_NOT_EXHAUSTIVE),
            ("String", randomUnionArrayElement, valueOrErrorMessageArgBeforeFunction, MATCHING_NOT_EXHAUSTIVE),
            ("String", randomStringArrayElement, invalidValueOrErrorMessage, valueOrErrorInvalidFunctionMessage),
            ("ByteVector", randomByteVectorArrayElement, invalidValueOrErrorMessageArgBeforeFunc, valueOrErrorInvalidFunctionMessage)
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(firstData, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
