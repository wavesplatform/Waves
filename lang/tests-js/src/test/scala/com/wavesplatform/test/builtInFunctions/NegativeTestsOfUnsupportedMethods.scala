package com.wavesplatform.test.builtInFunctions

import com.wavesplatform.JsTestBase
import com.wavesplatform.lang.directives.values.V3
import testHelpers.GeneratorContractsForBuiltInFunctions
import testHelpers.RandomDataGenerator.{randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomStringArrayElement}
import testHelpers.TestDataConstantsAndMethods.{
  CANT_FIND_FUNCTION,
  GreaterV3ResultBinaryEntry,
  UNDEFINED_TYPE,
  intList,
  oldVersions,
  rideV3Result,
  stringList
}
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
    test("toBigInt functions compilation error: Undefined type: `BigInt` for ride v3, v4") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("BigInt", version)
        for (
          (data, function, error) <- Seq(
            (randomInt.toString, toBigInt, UNDEFINED_TYPE),
            (randomStringArrayElement, toBigIntArgBeforeFunc, UNDEFINED_TYPE)
          )
        ) {
          val script = precondition.onlyMatcherContract(data, function)
          assertCompileErrorDApp(script, version, error)
        }
      }
    }

    test("compilation error: invalid data invoke for ride v3, v4 (argument before function)") {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, invokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
      }
    }

    test("compilation error: blake2b256 functions Can't find a function for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      for (
        (data, function, error) <- Seq(
          (randomByteVectorArrayElement, blake2b256_16Kb, CANT_FIND_FUNCTION),
          (randomByteVectorArrayElement, blake2b256_32Kb, CANT_FIND_FUNCTION),
          (randomByteVectorArrayElement, blake2b256_64Kb, CANT_FIND_FUNCTION),
          (randomByteVectorArrayElement, blake2b256_128Kb, CANT_FIND_FUNCTION)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
        assertCompileErrorDApp(script, V3, error)
      }
    }

    test("compilation error: keccak256 Can't find a functions for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("ByteVector", V3)
      for (
        (data, function, error) <- Seq(
          (randomByteVectorArrayElement, keccak256_16Kb, CANT_FIND_FUNCTION),
          (randomByteVectorArrayElement, keccak256_32Kb, CANT_FIND_FUNCTION),
          (randomByteVectorArrayElement, keccak256_64Kb, CANT_FIND_FUNCTION),
          (randomByteVectorArrayElement, keccak256_128Kb, CANT_FIND_FUNCTION)
        )
      ) {
        val script = precondition.codeFromMatchingAndCase(data, function, rideV3Result, GreaterV3ResultBinaryEntry)
        assertCompileErrorDApp(script, V3, error)
      }
    }

    test("Can't find a functions for V3") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      for (
        (data, list, function) <- Seq(
          (randomStringArrayElement, stringList, containsElement),
          (randomInt.toString, intList, indexOf),
          (randomInt.toString, intList, max),
          (randomInt.toString, intList, min),
          (randomInt.toString, intList, removeByIndex)
        )
      ) {
        val script = precondition.simpleRideCode(data, list, function)
        assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
    }
  }
}
