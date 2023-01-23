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
import utest.{Tests, test}

object FromBaseString extends JsTestBase {
  private val fromBase16String              = "fromBase16String(callerTestData)"
  private val fromBase16StringArgBeforeFunc = "callerTestData.fromBase16String()"
  private val fromBase58String              = "fromBase58String(callerTestData)"
  private val fromBase58StringArgBeforeFunc = "callerTestData.fromBase58String()"
  private val fromBase64String              = "fromBase64String(callerTestData)"
  private val fromBase64StringArgBeforeFunc = "callerTestData.fromBase64String()"
  private val invalidFromBase16String       = "fromBase16String()"
  private val invalidFromBase58String       = "fromBase58String()"
  private val invalidFromBase64String       = "fromBase64String()"
  private val invalidFromBase16StringArgBeforeFunction = "callerTestData.fromBase16String(callerTestData)"
  private val invalidFromBase58StringArgBeforeFunction = "callerTestData.fromBase58String(callerTestData)"
  private val invalidFromBase64StringArgBeforeFunction = "callerTestData.fromBase64String(callerTestData)"
  private val invalidErrorForFromBase16String   = testData.invalidFunctionError("fromBase16String", 1)
  private val invalidErrorForFromBase58String   = testData.invalidFunctionError("fromBase58String", 1)
  private val invalidErrorForFromBase64String   = testData.invalidFunctionError("fromBase64String", 1)

  val tests: Tests = Tests {
    test("check: function fromBase16String compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          fromBase16String
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function fromBase16String compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          fromBase16StringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function fromBase58String compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          fromBase58String
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function fromBase58String compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          fromBase58StringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function fromBase64String compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          fromBase64String
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test("check: function fromBase64String compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          fromBase64StringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // invalid data
    test("compilation error: function fromBase16String invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomBoolean.toString,
          fromBase16String
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: function fromBase16String invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          fromBase16StringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: function fromBase58String invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          fromBase58String
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: function fromBase58String invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          fromBase58StringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: function fromBase64String invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          fromBase64String
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    test("compilation error: function fromBase64String invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          fromBase64StringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("String"))
      }
    }

    // invalid function
    test("compilation error: invalid function fromBase16String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFromBase16String
        )
        assertCompileErrorDApp(script, version, invalidErrorForFromBase16String)
      }
    }

    test("compilation error: invalid function fromBase58String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFromBase58String
        )
        assertCompileErrorDApp(script, version, invalidErrorForFromBase58String)
      }
    }

    test("compilation error: invalid function fromBase64String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFromBase64String
        )
        assertCompileErrorDApp(script, version, invalidErrorForFromBase64String)
      }
    }

    test("compilation error: invalid function fromBase16String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFromBase16StringArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, invalidErrorForFromBase16String)
      }
    }

    test("compilation error: invalid function fromBase58String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFromBase58StringArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, invalidErrorForFromBase58String)
      }
    }

    test("compilation error: invalid function fromBase64String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", version)
        val script = precondition.onlyMatcherContract(
          randomStringArrayElement,
          invalidFromBase64StringArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, invalidErrorForFromBase64String)
      }
    }
  }
}
