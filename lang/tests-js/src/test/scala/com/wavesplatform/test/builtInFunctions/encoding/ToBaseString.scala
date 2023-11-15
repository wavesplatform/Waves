package com.wavesplatform.test.builtInFunctions.encoding

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomDigestAlgorithmTypeArrayElement,
  randomInt,
  randomStringArrayElement,
  randomUnionArrayElement
}
import testHelpers.TestDataConstantsAndMethods.{actualVersions, nonMatchingTypes}
import utest.{Tests, test}

object ToBaseString extends JsTestBase {
  private val toBase16String                         = "toBase16String(callerTestData)"
  private val toBase16StringArgBeforeFunc            = "callerTestData.toBase16String()"
  private val toBase58String                         = "toBase58String(callerTestData)"
  private val toBase58StringArgBeforeFunc            = "callerTestData.toBase58String()"
  private val toBase64String                         = "toBase64String(callerTestData)"
  private val toBase64StringArgBeforeFunc            = "callerTestData.toBase64String()"
  private val invalidToBase16String                  = "toBase16String()"
  private val invalidToBase58String                  = "toBase58String()"
  private val invalidToBase64String                  = "toBase64String()"
  private val invalidToBase16StringArgBeforeFunction = "callerTestData.toBase16String(callerTestData)"
  private val invalidToBase58StringArgBeforeFunction = "callerTestData.toBase58String(callerTestData)"
  private val invalidToBase64StringArgBeforeFunction = "callerTestData.toBase64String(callerTestData)"
  private val invalidErrorForToBase16String          = testData.invalidFunctionError("toBase16String", 1)
  private val invalidErrorForToBase58String          = testData.invalidFunctionError("toBase58String", 1)
  private val invalidErrorForToBase64String          = testData.invalidFunctionError("toBase64String", 1)

  val tests: Tests = Tests {
    test("RIDE-120. Function toBaseString should compile for valid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, function) <- Seq(
            (randomByteVectorArrayElement, toBase16String),
            (randomByteVectorArrayElement, toBase16StringArgBeforeFunc),
            (randomByteVectorArrayElement, toBase58String),
            (randomByteVectorArrayElement, toBase58StringArgBeforeFunc),
            (randomByteVectorArrayElement, toBase64String),
            (randomByteVectorArrayElement, toBase64StringArgBeforeFunc)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-121. Function toBaseString should throw an error for invalid data") {
      for (version <- actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, function, error) <- Seq(
            (randomBoolean.toString, toBase16String, nonMatchingTypes("ByteVector")),
            (randomAliasDataArrayElement, toBase16StringArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomStringArrayElement, toBase58String, nonMatchingTypes("ByteVector")),
            (randomDigestAlgorithmTypeArrayElement, toBase58StringArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomInt.toString, toBase64String, nonMatchingTypes("ByteVector")),
            (randomUnionArrayElement, toBase64StringArgBeforeFunc, nonMatchingTypes("ByteVector")),
            (randomByteVectorArrayElement, invalidToBase16String, invalidErrorForToBase16String),
            (randomByteVectorArrayElement, invalidToBase58String, invalidErrorForToBase58String),
            (randomByteVectorArrayElement, invalidToBase64String, invalidErrorForToBase64String),
            (randomByteVectorArrayElement, invalidToBase16StringArgBeforeFunction, invalidErrorForToBase16String),
            (randomByteVectorArrayElement, invalidToBase58StringArgBeforeFunction, invalidErrorForToBase58String),
            (randomByteVectorArrayElement, invalidToBase64StringArgBeforeFunction, invalidErrorForToBase64String)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }
  }
}
