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
import utest.{Tests, test}

object ToString extends JsTestBase {
  // toString
  private val toStr                     = "toString(callerTestData)"
  private val toStrArgBeforeFunc        = "callerTestData.toString()"
  private val invalidToStr              = "toString()"
  private val invalidToStrArgBeforeFunc = "callerTestData.toString(callerTestData)"

  val tests: Tests = Tests {
    test.apply("check: toString function compiles with a address data type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomAddressDataArrayElement,
          toStr
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a Integer data type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          toStr
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a Boolean data type") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomBoolean.toString,
          toStr
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a BigInt data type") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          s"toBigInt($randomInt)",
          toStr
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a address data type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomAddressDataArrayElement,
          toStrArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a Integer data type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomInt.toString,
          toStrArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a Boolean data type (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomBoolean.toString,
          toStrArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: toString function compiles with a Boolean data type (argument before function)") {
      for (version <- testData.versionsSupportingTheNewFeatures) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          s"toBigInt($randomInt)",
          toStrArgBeforeFunc
        )
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload toString - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          toStr
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload toString (argument before function) - invalid data") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomDigestAlgorithmTypeArrayElement,
          toStrArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload toString - invalid function") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          invalidToStr
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }

    test.apply("compilation error: Can't find a function overload toString (argument before function) - invalid function") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        val script = precondition.onlyMatcherContract(
          randomUnionArrayElement,
          invalidToStrArgBeforeFunc
        )
        assertCompileErrorDApp(script, version, testData.CANT_FIND_A_FUNCTION_OVERLOAD)
      }
    }
  }
}
