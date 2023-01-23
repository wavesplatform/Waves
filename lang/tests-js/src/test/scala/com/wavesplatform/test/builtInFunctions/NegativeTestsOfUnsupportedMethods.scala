package com.wavesplatform.test.builtInFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{intList, stringList}
import utest.{Tests, test}

object NegativeTestsOfUnsupportedMethods extends JsTestBase {
  private val toBigInt              = "toBigInt(callerTestData)"
  private val toBigIntArgBeforeFunc = "callerTestData.toBigInt()"
  private val invokeArgBeforeFunc   = "addressFromStringValue(dapp2).invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"

  private val blake2b256_16Kb  = "blake2b256_16Kb(callerTestData)"
  private val blake2b256_32Kb  = "blake2b256_32Kb(callerTestData)"
  private val blake2b256_64Kb  = "blake2b256_64Kb(callerTestData)"
  private val blake2b256_128Kb = "blake2b256_128Kb(callerTestData)"
  private val keccak256_16Kb   = "keccak256_16Kb(callerTestData)"
  private val keccak256_32Kb   = "keccak256_32Kb(callerTestData)"
  private val keccak256_64Kb   = "keccak256_64Kb(callerTestData)"
  private val keccak256_128Kb  = "keccak256_128Kb(callerTestData)"

  private val containsElement = "containsElement(foo, bar)"
  private val indexOf         = "indexOf(bar, foo)"
  private val removeByIndex   = "removeByIndex(bar, foo)"

  private val max = "max(callerTestData)"
  private val min = "min(callerTestData)"

  val tests: Tests = Tests {
    test("compilation error: Undefined type: `BigInt` for ride v3, v4") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomInt.toString, toBigInt)
        assertCompileErrorDApp(script, version, testData.UNDEFINED_TYPE)
      }
    }

    test("compilation error: Undefined type: `BigInt` for ride v3, v4 (argument before function)") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        val script       = precondition.onlyMatcherContract(randomStringArrayElement, toBigIntArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.UNDEFINED_TYPE)
      }
    }

    test("compilation error: invalid data invoke for ride v3, v4 (argument before function)") {
      for (version <- testData.oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, invokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, testData.CANT_FIND_FUNCTION)
      }
    }

    test("compilation error: blake2b256_16Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        blake2b256_16Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: blake2b256_32Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        blake2b256_32Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: blake2b256_64Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        blake2b256_64Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: blake2b256_128Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        blake2b256_128Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: keccak256_16Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        keccak256_16Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: keccak256_32Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        keccak256_32Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: keccak256_64Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        keccak256_64Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: keccak256_128Kb Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      val script = precondition.codeFromMatchingAndCase(
        randomByteVectorArrayElement,
        keccak256_128Kb,
        testData.rideV3Result,
        testData.GreaterV3ResultBinaryEntry
      )
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: containsElement Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, containsElement)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: indexOf Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, indexOf)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: max Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, max)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: max Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, min)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }

    test("compilation error: removeByIndex Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, removeByIndex)
      assertCompileErrorDApp(script, V3, testData.CANT_FIND_FUNCTION)
    }
  }
}
