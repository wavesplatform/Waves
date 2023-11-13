package com.wavesplatform.test.builtInFunctions.list

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V4
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomDigestAlgorithmTypeArrayElement, randomInt, randomUnionArrayElement}
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_A_FUNCTION_OVERLOAD,
  actualVersionsWithoutV3,
  intList,
  invalidFunctionError,
  nonMatchingTypes,
  versionsSupportingTheNewFeatures
}
import utest.{Tests, test}

object Min extends JsTestBase {
  private val min                       = "min(callerTestData)"
  private val minArgBeforeFunc          = "callerTestData.min()"
  private val invalidMin                = "min()"
  private val invalidMinArgBeforeFunc   = "callerTestData.min(callerTestData)"
  private val minForBigInt              = "min([callerTestData])"
  private val minForBigIntArgBeforeFunc = "[callerTestData].min()"
  private val invalidMinForBigInt       = "[callerTestData].min([callerTestData], [callerTestData])"

  val tests: Tests = Tests {
    test("RIDE-165. Function Min should compile with a list") {
      for (version <- actualVersionsWithoutV3) {
        for (
          (data, function, dataType) <- Seq(
            (intList, min, "Int"),
            (intList, minArgBeforeFunc, "Int")
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-166. Function Min should compile with a BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        for (
          (data, function, dataType) <- Seq(
            (s"toBigInt($randomInt)", minForBigInt, "BigInt"),
            (s"toBigInt($randomInt)", minForBigIntArgBeforeFunc, "BigInt")
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-167. Function Min should throw an error for invalid data or type") {
      for (version <- actualVersionsWithoutV3) {
        for (
          (data, function, dataType, error) <- Seq(
            (randomUnionArrayElement, min, "Int", nonMatchingTypes("List[Int]")),
            (randomDigestAlgorithmTypeArrayElement, minArgBeforeFunc, "Int", nonMatchingTypes("List[Int]")),
            (intList, invalidMin, "Int", invalidFunctionError("min", 1)),
            (intList, invalidMinArgBeforeFunc, "Int", invalidFunctionError("min", 1)),
            (intList, invalidMinForBigInt, "Int", invalidFunctionError("min", 1))
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          if (version == V4) {
            assertCompileErrorDApp(script, version, error)
          } else {
            assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
          }
        }
      }
    }
  }
}
