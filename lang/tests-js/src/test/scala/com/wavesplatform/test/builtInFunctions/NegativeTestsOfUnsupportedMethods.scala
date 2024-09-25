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
    test("RIDE-280. toBigInt function should throw a compilation error for RIDE versions V3 and V4.") {
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

    test(
      "RIDE-281. compilation error 'invalid data invoke' should occur for RIDE versions V3 and V4 when an argument is placed before the function."
    ) {
      for (version <- oldVersions) {
        val precondition = new GeneratorContractsForBuiltInFunctions("", version)
        val script       = precondition.codeForDAppInvocation(randomByteVectorArrayElement, randomAddressDataArrayElement, invokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
      }
    }

    test("RIDE-282. blake2b256 functions should throw an error for RIDE version V3.") {
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

    test("RIDE-283. keccak256 functions should throw an error for RIDE version V3.") {
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

    test("RIDE-284. containsElement functions should throw an error for RIDE version V3.") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomStringArrayElement, stringList, containsElement)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }

    test("RIDE-285. indexOf functions should throw an error for RIDE version V3.") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, indexOf)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }

    test("RIDE-286. max functions should throw an error for RIDE version V3.") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, max)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }

    test("RIDE-287. min functions should throw an error for RIDE version V3.") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, min)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }

    test("RIDE-288. removeByIndex functions should throw an error for RIDE version V3.") {
      val precondition = new GeneratorContractsForBuiltInFunctions("", V3)
      val script       = precondition.simpleRideCode(randomInt.toString, intList, removeByIndex)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
    }
  }
}
