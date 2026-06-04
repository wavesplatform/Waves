import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("TransferTransactionFromProto", () => {
  const transferTransactionFromProto = "transferTransactionFromProto(callerTestData)"
  const transferTransactionFromProtoArgBeforeFunc = "callerTestData.transferTransactionFromProto()"
  const invalidTransferTransactionFromProto = "transferTransactionFromProto()"
  const invalidTransferTransactionFromProtoArgBeforeFunction = "transferTransactionFromProto()"

    test("RIDE-85. transferTransactionFromProto function should compile for Issue V4 and more", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("TransferTransaction", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), transferTransactionFromProto], [randomByteVectorArrayElement(), transferTransactionFromProtoArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-86. transferTransactionFromProto function throws an error for invalid values for V4 and more", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("TransferTransaction", version)
        for (const [data, func, error] of [[randomDigestAlgorithmTypeArrayElement(), transferTransactionFromProto, nonMatchingTypes("ByteVector")], [randomDigestAlgorithmTypeArrayElement(), transferTransactionFromProtoArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidTransferTransactionFromProto, invalidFunctionError("transferTransactionFromProto", 1)], [randomByteVectorArrayElement(), invalidTransferTransactionFromProtoArgBeforeFunction, invalidFunctionError("transferTransactionFromProto", 1)]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-87. transferTransactionFromProto function should throw a compilation error for Ride V3", () => {
      const precondition = new ContractGenerator("TransferTransaction", V3)
      for (const [data, func] of [[randomByteVectorArrayElement(), transferTransactionFromProto], [randomByteVectorArrayElement(), transferTransactionFromProtoArgBeforeFunc]]) {
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
  });
});
