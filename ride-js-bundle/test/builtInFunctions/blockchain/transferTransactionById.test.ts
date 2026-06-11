import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomStringArrayElement } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("TransferTransactionById", () => {
  const transferTransactionById = "transferTransactionById(callerTestData)"
  const transferTransactionByIdArgBeforeFunc = "callerTestData.transferTransactionById()"

  const invalidTransferTransactionById = "transferTransactionById()"
  const invalidTransferTransactionByIdArg = `callerTestData.transferTransactionById(callerTestData)`

    test("RIDE-44. TransactionHeightById function should compile", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Unit", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), transferTransactionById], [randomByteVectorArrayElement(), transferTransactionByIdArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-45. Negative cases for TransactionHeightById function", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Unit", version)
        for (const [data, func, error] of [[randomDigestAlgorithmTypeArrayElement(), transferTransactionById, nonMatchingTypes("ByteVector")], [randomStringArrayElement(), transferTransactionByIdArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidTransferTransactionById, invalidFunctionError("transferTransactionById", 1)], [randomByteVectorArrayElement(), invalidTransferTransactionByIdArg, invalidFunctionError("transferTransactionById", 1)]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
