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

object Max extends JsTestBase {
  private val max                       = "max(callerTestData)"
  private val maxArgBeforeFunc          = "callerTestData.max()"
  private val invalidMax                = "max()"
  private val invalidMaxArgBeforeFunc   = "callerTestData.max(callerTestData)"
  private val maxForBigInt              = "max([callerTestData])"
  private val maxForBigIntArgBeforeFunc = "[callerTestData].max()"
  private val invalidMaxForBigInt       = "[callerTestData].max([callerTestData], [callerTestData])"

  val tests: Tests = Tests {
    test("RIDE-162. Function Max should compile with a list") {
      for (version <- actualVersionsWithoutV3) {
        for (
          (data, function, dataType) <- Seq(
            (intList, max, "Int"),
            (intList, maxArgBeforeFunc, "Int")
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-163. Function Max should compile with a BigInt") {
      for (version <- versionsSupportingTheNewFeatures) {
        for (
          (data, function, dataType) <- Seq(
            (s"toBigInt($randomInt)", maxForBigInt, "BigInt"),
            (s"toBigInt($randomInt)", maxForBigIntArgBeforeFunc, "BigInt")
          )
        ) {
          val precondition = new GeneratorContractsForBuiltInFunctions(dataType, version)
          val script       = precondition.onlyMatcherContract(data, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-164. Function Max should throw an error for invalid data or type") {
      for (version <- actualVersionsWithoutV3) {
        for (
          (data, function, dataType, error) <- Seq(
            (randomUnionArrayElement, max, "Int", nonMatchingTypes("List[Int]")),
            (randomDigestAlgorithmTypeArrayElement, maxArgBeforeFunc, "Int", nonMatchingTypes("List[Int]")),
            (intList, invalidMax, "Int", invalidFunctionError("max", 1)),
            (intList, invalidMaxArgBeforeFunc, "Int", invalidFunctionError("max", 1)),
            (intList, invalidMaxForBigInt, "Int", invalidFunctionError("max", 1))
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
