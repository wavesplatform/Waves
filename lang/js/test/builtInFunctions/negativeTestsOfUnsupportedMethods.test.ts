import { describe, test } from "vitest";
import { assertCompileErrorDApp, V3, V4 } from "../helpers/jsTestBase";
import { ContractGenerator } from "../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomStringArrayElement } from "../helpers/randomData";
import { CANT_FIND_FUNCTION, GreaterV3ResultBinaryEntry, UNDEFINED_TYPE, intList, oldVersions, rideV3Result, stringList } from "../helpers/testData";

describe("NegativeTestsOfUnsupportedMethods", () => {
  const toBigInt = "toBigInt(callerTestData)"
  const toBigIntArgBeforeFunc = "callerTestData.toBigInt()"
  const invokeArgBeforeFunc = "addressFromStringValue(dapp2).invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"

  const blake2b256_16Kb = "blake2b256_16Kb(callerTestData)"
  const blake2b256_32Kb = "blake2b256_32Kb(callerTestData)"
  const blake2b256_64Kb = "blake2b256_64Kb(callerTestData)"
  const blake2b256_128Kb = "blake2b256_128Kb(callerTestData)"
  const keccak256_16Kb = "keccak256_16Kb(callerTestData)"
  const keccak256_32Kb = "keccak256_32Kb(callerTestData)"
  const keccak256_64Kb = "keccak256_64Kb(callerTestData)"
  const keccak256_128Kb = "keccak256_128Kb(callerTestData)"

  const containsElement = "containsElement(foo, bar)"
  const indexOf = "indexOf(bar, foo)"
  const removeByIndex = "removeByIndex(bar, foo)"

  const max = "max(callerTestData)"
  const min = "min(callerTestData)"

    test("RIDE-280. toBigInt function should throw a compilation error for RIDE versions V3 and V4.", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("BigInt", version)
        for (const [data, func, error] of [[randomInt(), toBigInt, UNDEFINED_TYPE], [randomStringArrayElement(), toBigIntArgBeforeFunc, UNDEFINED_TYPE]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-281. compilation error 'invalid data invoke' should occur for RIDE versions V3 and V4 when an argument is placed before the function.", () => {
      for (const version of oldVersions) {
        const precondition = new ContractGenerator("", version)
        const script = precondition.codeForDAppInvocation(randomByteVectorArrayElement(), randomAddressDataArrayElement(), invokeArgBeforeFunc)
        assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
      }
  });

    test("RIDE-282. blake2b256 functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("ByteVector", V3)
      for (const [data, func, error] of [[randomByteVectorArrayElement(), blake2b256_16Kb, CANT_FIND_FUNCTION], [randomByteVectorArrayElement(), blake2b256_32Kb, CANT_FIND_FUNCTION], [randomByteVectorArrayElement(), blake2b256_64Kb, CANT_FIND_FUNCTION], [randomByteVectorArrayElement(), blake2b256_128Kb, CANT_FIND_FUNCTION]]) {
        const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
        assertCompileErrorDApp(script, V3, error)
      }
  });

    test("RIDE-283. keccak256 functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("ByteVector", V3)
      for (const [data, func, error] of [[randomByteVectorArrayElement(), keccak256_16Kb, CANT_FIND_FUNCTION], [randomByteVectorArrayElement(), keccak256_32Kb, CANT_FIND_FUNCTION], [randomByteVectorArrayElement(), keccak256_64Kb, CANT_FIND_FUNCTION], [randomByteVectorArrayElement(), keccak256_128Kb, CANT_FIND_FUNCTION]]) {
        const script = precondition.codeFromMatchingAndCase(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
        assertCompileErrorDApp(script, V3, error)
      }
  });

    test("RIDE-284. containsElement functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("", V3)
      const script = precondition.simpleRideCode(randomStringArrayElement(), stringList, containsElement)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });

    test("RIDE-285. indexOf functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("", V3)
      const script = precondition.simpleRideCode(randomInt(), intList, indexOf)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });

    test("RIDE-286. max functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("", V3)
      const script = precondition.simpleRideCode(randomInt(), intList, max)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });

    test("RIDE-287. min functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("", V3)
      const script = precondition.simpleRideCode(randomInt(), intList, min)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });

    test("RIDE-288. removeByIndex functions should throw an error for RIDE version V3.", () => {
      const precondition = new ContractGenerator("", V3)
      const script = precondition.simpleRideCode(randomInt(), intList, removeByIndex)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
