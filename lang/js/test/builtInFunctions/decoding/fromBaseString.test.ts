import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("FromBaseString", () => {
  const fromBase16String = "fromBase16String(callerTestData)"
  const fromBase16StringArgBeforeFunc = "callerTestData.fromBase16String()"
  const fromBase58String = "fromBase58String(callerTestData)"
  const fromBase58StringArgBeforeFunc = "callerTestData.fromBase58String()"
  const fromBase64String = "fromBase64String(callerTestData)"
  const fromBase64StringArgBeforeFunc = "callerTestData.fromBase64String()"
  const invalidFromBase16String = "fromBase16String()"
  const invalidFromBase58String = "fromBase58String()"
  const invalidFromBase64String = "fromBase64String()"
  const invalidFromBase16StringArgBeforeFunction = "callerTestData.fromBase16String(callerTestData)"
  const invalidFromBase58StringArgBeforeFunction = "callerTestData.fromBase58String(callerTestData)"
  const invalidFromBase64StringArgBeforeFunction = "callerTestData.fromBase64String(callerTestData)"
  const invalidErrorForFromBase16String = invalidFunctionError("fromBase16String", 1)
  const invalidErrorForFromBase58String = invalidFunctionError("fromBase58String", 1)
  const invalidErrorForFromBase64String = invalidFunctionError("fromBase64String", 1)

    test("RIDE-118. Function fromBaseString should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomStringArrayElement(), fromBase16String], [randomStringArrayElement(), fromBase16StringArgBeforeFunc], [randomStringArrayElement(), fromBase58String], [randomStringArrayElement(), fromBase58StringArgBeforeFunc], [randomStringArrayElement(), fromBase64String], [randomStringArrayElement(), fromBase64StringArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    // invalid data
    test("RIDE-119. Function fromBaseString should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomBoolean(), fromBase16String, nonMatchingTypes("String")], [randomAliasDataArrayElement(), fromBase16StringArgBeforeFunc, nonMatchingTypes("String")], [randomByteVectorArrayElement(), fromBase58String, nonMatchingTypes("String")], [randomDigestAlgorithmTypeArrayElement(), fromBase58StringArgBeforeFunc, nonMatchingTypes("String")], [randomByteVectorArrayElement(), fromBase64String, nonMatchingTypes("String")], [randomUnionArrayElement(), fromBase64StringArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), invalidFromBase16String, invalidErrorForFromBase16String], [randomStringArrayElement(), invalidFromBase58String, invalidErrorForFromBase58String], [randomStringArrayElement(), invalidFromBase64String, invalidErrorForFromBase64String], [randomStringArrayElement(), invalidFromBase16StringArgBeforeFunction, invalidErrorForFromBase16String], [randomStringArrayElement(), invalidFromBase58StringArgBeforeFunction, invalidErrorForFromBase58String], [randomStringArrayElement(), invalidFromBase64StringArgBeforeFunction, invalidErrorForFromBase64String]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
