import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("AddressFromPublicKey", () => {
  const addressFromPublicKey = `addressFromPublicKey(callerTestData)`
  const addressFromPublicKeyArgBeforeFunction = `callerTestData.addressFromPublicKey()`
  const invalidAddressFromPublicKey = `addressFromPublicKey()`
  const invalidAddressFromPublicKeyArgBeforeFunc = `callerTestData.addressFromPublicKey(callerTestData, callerTestData)`
  const invalidAddressFromPublicKeyData = `addressFromPublicKey(callerTestData, ${randomUnionArrayElement()})`

    test("RIDE-60. AddressFromPublicKey function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Address", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), addressFromPublicKey], [randomByteVectorArrayElement(), addressFromPublicKeyArgBeforeFunction]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-61. AddressFromPublicKey function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomAddressDataArrayElement(), addressFromPublicKey, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), addressFromPublicKeyArgBeforeFunction, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidAddressFromPublicKey, invalidFunctionError("addressFromPublicKey", 1)], [randomAddressDataArrayElement(), invalidAddressFromPublicKeyArgBeforeFunc, invalidFunctionError("addressFromPublicKey", 1)], [randomAddressDataArrayElement(), invalidAddressFromPublicKeyData, invalidFunctionError("addressFromPublicKey", 1)]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
