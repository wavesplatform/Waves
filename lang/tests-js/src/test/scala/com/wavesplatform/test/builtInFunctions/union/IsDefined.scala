package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
import utest.{Tests, test}

object IsDefined extends JsTestBase {
  // isDefined
  private val isDefined                     = "isDefined(callerTestData)"
  private val isDefinedArgBeforeFunc        = "callerTestData.isDefined()"
  private val invalidIsDefined              = "isDefined()"
  private val invalidIsDefinedArgBeforeFunc = "callerTestData.isDefined(callerTestData)"
  private val invalidErrorIsDefined         = testData.invalidFunctionError("isDefined", 1)

  val tests: Tests = Tests {
    test.apply("check: isDefined function compiles") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, isDefined)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: isDefined function compiles(argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomDigestAlgorithmTypeArrayElement, isDefinedArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: Can't find a function overload isDefined") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidIsDefined)
        assertCompileErrorDApp(script, version, invalidErrorIsDefined)
      }
    }

    test.apply("compilation error: Can't find a function overload isDefined (argument before function)") {
      for (version <- testData.actualVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("Boolean", version)
        val script       = precondition.onlyMatcherContract(randomUnionArrayElement, invalidIsDefinedArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorIsDefined)
      }
    }
  }

}
