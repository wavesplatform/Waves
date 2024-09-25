package com.wavesplatform.test.builtInFunctions.string

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAliasDataArrayElement, randomInt, randomIssuesArrayElement, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{actualVersionsWithoutV3, nonMatchingTypes}
import utest.{Tests, test}

object Contains extends JsTestBase {
  private val contains                     = "contains(bar, foo)"
  private val containsArgBeforeFunc        = "bar.contains(foo)"
  private val invalidContains              = "contains(foo)"
  private val invalidContainsArgBeforeFunc = "foo.contains(bar, foo)"
  private val invalidErrorContains         = testData.invalidFunctionError("contains", 2)

  val tests: Tests = Tests {
    test("RIDE-208. function contains should compile for valid data") {
      for (version <- actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function) <- Seq(
            (randomStringArrayElement, randomStringArrayElement, contains),
            (randomStringArrayElement, randomStringArrayElement, containsArgBeforeFunc)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileSuccessDApp(script, version)
        }
      }
    }

    test("RIDE-209. function contains throw a compilation error for invalid data") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("String", version)
        for (
          (data, list, function, error) <- Seq(
            (randomInt.toString, randomAliasDataArrayElement, containsArgBeforeFunc, nonMatchingTypes("String")),
            (randomInt.toString, randomIssuesArrayElement, containsArgBeforeFunc, nonMatchingTypes("String")),
            (randomStringArrayElement, randomStringArrayElement, invalidContains, invalidErrorContains),
            (randomStringArrayElement, randomStringArrayElement, invalidContainsArgBeforeFunc, invalidErrorContains)
          )
        ) {
          val script = precondition.simpleRideCode(data, list, function)
          assertCompileErrorDApp(script, version, error)
        }

      }
    }

    test("RIDE-210. Can't find a function contains for RIDE V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("String", V3)
      val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, contains)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
