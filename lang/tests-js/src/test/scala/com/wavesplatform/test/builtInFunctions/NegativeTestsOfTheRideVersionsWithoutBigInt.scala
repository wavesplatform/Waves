package com.wavesplatform.test.builtInFunctions

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomInt, randomStringArrayElement}
import utest.{Tests, test}

object NegativeTestsOfTheRideVersionsWithoutBigInt extends JsTestBase {
  private val toBigInt = "toBigInt(callerTestData)"
  private val toBigIntArgBeforeFunc = "callerTestData.toBigInt()"

  val tests: Tests = Tests {
    test.apply("compilation error: Undefined type: `BigInt` for ride v3, v4") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomInt.toString, toBigInt)
        assertCompileErrorDApp(script, version, testData.UNDEFINED_TYPE)
      }
    }

    test.apply("compilation error: Undefined type: `BigInt` for ride v3, v4 (argument before function)") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script = precondition.onlyMatcherContract(randomStringArrayElement, toBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.UNDEFINED_TYPE)
      }
    }
  }
}
