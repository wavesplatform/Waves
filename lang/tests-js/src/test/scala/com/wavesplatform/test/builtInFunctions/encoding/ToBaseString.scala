package com.wavesplatform.test.builtInFunctions.encoding

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
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
    test.apply("check: function toBase16String compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          toBase16String
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function toBase16String compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          toBase16StringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function toBase58String compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          toBase58String
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function toBase58String compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          toBase58StringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function toBase64String compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          toBase64String
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: function toBase64String compiles (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          toBase64StringArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    // invalid data
    test.apply("compilation error: function toBase16String invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomBoolean.toString,
          toBase16String
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function toBase16String invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomAliasDataArrayElement,
          toBase16StringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function toBase58String invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          toBase58String
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function toBase58String invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          toBase58StringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function toBase64String invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomBoolean.toString,
          toBase64String
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    test.apply("compilation error: function toBase64String invalid data (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          toBase64StringArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.nonMatchingTypes("ByteVector"))
      }
    }

    // invalid function
    test.apply("compilation error: invalid function toBase16String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidToBase16String
        )
        assertCompileErrorDApp(script, version, invalidErrorForToBase16String)
      }
    }

    test.apply("compilation error: invalid function toBase58String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidToBase58String
        )
        assertCompileErrorDApp(script, version, invalidErrorForToBase58String)
      }
    }

    test.apply("compilation error: invalid function toBase64String") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidToBase64String
        )
        assertCompileErrorDApp(script, version, invalidErrorForToBase64String)
      }
    }

    test.apply("compilation error: invalid function toBase16String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidToBase16StringArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, invalidErrorForToBase16String)
      }
    }

    test.apply("compilation error: invalid function toBase58String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidToBase58StringArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, invalidErrorForToBase58String)
      }
    }

    test.apply("compilation error: invalid function toBase64String (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomByteVectorArrayElement,
          invalidToBase64StringArgBeforeFunction
        )
        assertCompileErrorDApp(script, version, invalidErrorForToBase64String)
      }
    }
  }
}
