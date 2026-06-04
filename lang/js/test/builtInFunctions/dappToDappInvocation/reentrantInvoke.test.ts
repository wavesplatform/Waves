import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("ReentrantInvoke", () => {
  const reentrantInvoke = "reentrantInvoke(addressFromStringValue(dapp2),\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  const reentrantInvokeArgBeforeFunc = "addressFromStringValue(dapp2).reentrantInvoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  const invalidReentrantInvokeFunction = "reentrantInvoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  const invalidReentrantInvokeArgBeforeFunc = "addressFromStringValue(dapp2).reentrantInvoke([AttachedPayment(byteVector, payment)])"
  const invalidFunctionErrorResult = invalidFunctionError("reentrantInvoke", 4)

    test("RIDE-90. ReentrantInvoke function should compile for Issue RIDE V5 and more", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("", version)
        for (const [byteVector, data, func] of [[randomByteVectorArrayElement(), randomInt(), reentrantInvoke], [randomByteVectorArrayElement(), randomInt(), reentrantInvokeArgBeforeFunc]]) {
          const script = precondition.codeForDAppInvocation(byteVector, data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-91. ReentrantInvoke function throws an error for invalid values for RIDE V5 and more", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("", version)
        for (const [byteVector, data, func, error] of [[randomByteVectorArrayElement(), randomAddressDataArrayElement(), reentrantInvoke, nonMatchingTypes("Int")], [randomByteVectorArrayElement(), randomDigestAlgorithmTypeArrayElement(), reentrantInvokeArgBeforeFunc, nonMatchingTypes("Int")], [randomStringArrayElement(), randomInt(), reentrantInvoke, nonMatchingTypes("ByteVector|Unit")], [randomStringArrayElement(), randomInt(), reentrantInvokeArgBeforeFunc, nonMatchingTypes("ByteVector|Unit")], [randomByteVectorArrayElement(), randomInt(), invalidReentrantInvokeFunction, invalidFunctionErrorResult], [randomByteVectorArrayElement(), randomInt(), invalidReentrantInvokeArgBeforeFunc, invalidFunctionErrorResult]]) {
          const script = precondition.codeForDAppInvocation(byteVector, data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
