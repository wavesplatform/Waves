import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("ToBaseString", () => {
  const toBase16String = "toBase16String(callerTestData)"
  const toBase16StringArgBeforeFunc = "callerTestData.toBase16String()"
  const toBase58String = "toBase58String(callerTestData)"
  const toBase58StringArgBeforeFunc = "callerTestData.toBase58String()"
  const toBase64String = "toBase64String(callerTestData)"
  const toBase64StringArgBeforeFunc = "callerTestData.toBase64String()"
  const invalidToBase16String = "toBase16String()"
  const invalidToBase58String = "toBase58String()"
  const invalidToBase64String = "toBase64String()"
  const invalidToBase16StringArgBeforeFunction = "callerTestData.toBase16String(callerTestData)"
  const invalidToBase58StringArgBeforeFunction = "callerTestData.toBase58String(callerTestData)"
  const invalidToBase64StringArgBeforeFunction = "callerTestData.toBase64String(callerTestData)"
  const invalidErrorForToBase16String = invalidFunctionError("toBase16String", 1)
  const invalidErrorForToBase58String = invalidFunctionError("toBase58String", 1)
  const invalidErrorForToBase64String = invalidFunctionError("toBase64String", 1)

    test("RIDE-120. Function toBaseString should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), toBase16String], [randomByteVectorArrayElement(), toBase16StringArgBeforeFunc], [randomByteVectorArrayElement(), toBase58String], [randomByteVectorArrayElement(), toBase58StringArgBeforeFunc], [randomByteVectorArrayElement(), toBase64String], [randomByteVectorArrayElement(), toBase64StringArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-121. Function toBaseString should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("String", version)
        for (const [data, func, error] of [[randomBoolean(), toBase16String, nonMatchingTypes("ByteVector")], [randomAliasDataArrayElement(), toBase16StringArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomStringArrayElement(), toBase58String, nonMatchingTypes("ByteVector")], [randomDigestAlgorithmTypeArrayElement(), toBase58StringArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomInt(), toBase64String, nonMatchingTypes("ByteVector")], [randomUnionArrayElement(), toBase64StringArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidToBase16String, invalidErrorForToBase16String], [randomByteVectorArrayElement(), invalidToBase58String, invalidErrorForToBase58String], [randomByteVectorArrayElement(), invalidToBase64String, invalidErrorForToBase64String], [randomByteVectorArrayElement(), invalidToBase16StringArgBeforeFunction, invalidErrorForToBase16String], [randomByteVectorArrayElement(), invalidToBase58StringArgBeforeFunction, invalidErrorForToBase58String], [randomByteVectorArrayElement(), invalidToBase64StringArgBeforeFunction, invalidErrorForToBase64String]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
