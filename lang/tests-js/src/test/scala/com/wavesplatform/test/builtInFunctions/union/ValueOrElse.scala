package com.wavesplatform.test.builtInFunctions.union

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{
  randomAddressDataArrayElement,
  randomAliasDataArrayElement,
  randomBoolean,
  randomByteVectorArrayElement,
  randomInt,
  randomIssuesArrayElement,
  randomStringArrayElement,
}
import utest.{Tests, test}

object ValueOrElse extends JsTestBase {
  // valueOrElse
  private val valueOrElse                     = "valueOrElse(bar, foo)"
  private val valueOrElseArgBeforeFunc        = "bar.valueOrElse(foo)"
  private val invalidValueOrElse              = "valueOrElse(foo)"
  private val invalidValueOrElseArgBeforeFunc = "foo.valueOrElse(foo, bar)"
  private val invalidErrorValueOrElse         = testData.invalidFunctionError("valueOrElse", 2)

  val tests: Tests = Tests {
    test.apply("check: valueOrElse function compiles with String") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomStringArrayElement, randomStringArrayElement, valueOrElse)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: valueOrElse function compiles with Int") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomInt.toString, valueOrElse)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: valueOrElse function compiles with Alias") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomAliasDataArrayElement, randomAliasDataArrayElement, valueOrElse)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: valueOrElse function compiles with Address (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomAddressDataArrayElement, randomAddressDataArrayElement, valueOrElseArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: valueOrElse function compiles with byteVector (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomByteVectorArrayElement, randomByteVectorArrayElement, valueOrElseArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("check: valueOrElse function compiles with boolean (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomBoolean.toString, randomBoolean.toString, valueOrElseArgBeforeFunc)
        assertCompileSuccessDApp(script, version)
      }
    }

    test.apply("compilation error: valueOrElse - Non-matching types") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, valueOrElseArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_MATCH_INFERRED_TYPE)
      }
    }

    test.apply("compilation error: valueOrElse - Non-matching types (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomIssuesArrayElement, valueOrElseArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_MATCH_INFERRED_TYPE)
      }
    }

    test.apply("compilation error: Can't find a function overload valueOrElse") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidValueOrElse)
        assertCompileErrorDApp(script, version, invalidErrorValueOrElse)
      }
    }

    test.apply("compilation error: Can't find a function overload valueOrElse (argument before function)") {
      for (version <- testData.actualVersionsWithoutV3) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.simpleRideCode(randomInt.toString, randomAliasDataArrayElement, invalidValueOrElseArgBeforeFunc)
        assertCompileErrorDApp(script, version, invalidErrorValueOrElse)
      }
    }

    test.apply("compilation error: Can't find a function V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomAliasDataArrayElement, randomAliasDataArrayElement, valueOrElse)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)

    }

    test.apply("compilation error: Can't find a function V3 (argument before function)") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomAddressDataArrayElement, randomAddressDataArrayElement, valueOrElseArgBeforeFunc)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
