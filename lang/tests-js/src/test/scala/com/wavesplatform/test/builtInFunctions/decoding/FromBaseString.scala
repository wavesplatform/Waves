package com.wavesplatform.test.builtInFunctions.decoding

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, invalidFunctionError, nonMatchingTypes}
import utest.{Tests, test}

object FromBaseString extends JsTestBase {
  private val fromBase16String                         = "fromBase16String(callerTestData)"
  private val fromBase16StringArgBeforeFunc            = "callerTestData.fromBase16String()"
  private val fromBase58String                         = "fromBase58String(callerTestData)"
  private val fromBase58StringArgBeforeFunc            = "callerTestData.fromBase58String()"
  private val fromBase64String                         = "fromBase64String(callerTestData)"
  private val fromBase64StringArgBeforeFunc            = "callerTestData.fromBase64String()"
  private val invalidFromBase16String                  = "fromBase16String()"
  private val invalidFromBase58String                  = "fromBase58String()"
  private val invalidFromBase64String                  = "fromBase64String()"
  private val invalidFromBase16StringArgBeforeFunction = "callerTestData.fromBase16String(callerTestData)"
  private val invalidFromBase58StringArgBeforeFunction = "callerTestData.fromBase58String(callerTestData)"
  private val invalidFromBase64StringArgBeforeFunction = "callerTestData.fromBase64String(callerTestData)"
  private val invalidErrorForFromBase16String          = invalidFunctionError("fromBase16String", 1)
  private val invalidErrorForFromBase58String          = invalidFunctionError("fromBase58String", 1)
  private val invalidErrorForFromBase64String          = invalidFunctionError("fromBase64String", 1)

  val tests: Tests = Tests {
    test("RIDE-118. Function fromBaseString should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function) <- Seq(
            (randomStringArrayElement, fromBase16String),
            (randomStringArrayElement, fromBase16StringArgBeforeFunc),
            (randomStringArrayElement, fromBase58String),
            (randomStringArrayElement, fromBase58StringArgBeforeFunc),
            (randomStringArrayElement, fromBase64String),
            (randomStringArrayElement, fromBase64StringArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    // invalid data
    test("RIDE-119. Function fromBaseString should throw an error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        for (
          (data, function, error) <- Seq(
            (randomBoolean.toString, fromBase16String, nonMatchingTypes("String")),
            (randomAliasDataArrayElement, fromBase16StringArgBeforeFunc, nonMatchingTypes("String")),
            (randomByteVectorArrayElement, fromBase58String, nonMatchingTypes("String")),
            (randomDigestAlgorithmTypeArrayElement, fromBase58StringArgBeforeFunc, nonMatchingTypes("String")),
            (randomByteVectorArrayElement, fromBase64String, nonMatchingTypes("String")),
            (randomUnionArrayElement, fromBase64StringArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, invalidFromBase16String, invalidErrorForFromBase16String),
            (randomStringArrayElement, invalidFromBase58String, invalidErrorForFromBase58String),
            (randomStringArrayElement, invalidFromBase64String, invalidErrorForFromBase64String),
            (randomStringArrayElement, invalidFromBase16StringArgBeforeFunction, invalidErrorForFromBase16String),
            (randomStringArrayElement, invalidFromBase58StringArgBeforeFunction, invalidErrorForFromBase58String),
            (randomStringArrayElement, invalidFromBase64StringArgBeforeFunction, invalidErrorForFromBase64String)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
