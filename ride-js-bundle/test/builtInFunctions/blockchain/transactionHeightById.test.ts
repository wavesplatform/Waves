import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("TransactionHeightById", () => {
  const transactionHeightById = "transactionHeightById(callerTestData)"
  const transactionHeightByIdArgBeforeFunc = "callerTestData.transactionHeightById()"

  const invalidTransactionHeightById = "transactionHeightById()"
  const invalidTransactionHeightByIdArg = `callerTestData.transactionHeightById(callerTestData)`


    test("RIDE-42. TransactionHeightById function should compile", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), transactionHeightById], [randomByteVectorArrayElement(), transactionHeightByIdArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-43. Negative cases for TransactionHeightById function", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func, error] of [[randomDigestAlgorithmTypeArrayElement(), transactionHeightById, nonMatchingTypes("ByteVector")], [randomStringArrayElement(), transactionHeightByIdArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidTransactionHeightById, invalidFunctionError("transactionHeightById", 1)], [randomByteVectorArrayElement(), invalidTransactionHeightByIdArg, invalidFunctionError("transactionHeightById", 1)]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
