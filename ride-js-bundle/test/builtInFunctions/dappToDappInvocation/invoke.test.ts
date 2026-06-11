import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V5 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { invalidFunctionError, nonMatchingTypes, versionsSupportingTheNewFeatures } from "../../helpers/testData";

describe("Invoke", () => {
  const invoke = "invoke(addressFromStringValue(dapp2),\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  const invokeArgBeforeFunc = "addressFromStringValue(dapp2).invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  const invalidInvokeFunction = "invoke(\"bar\",[a],[AttachedPayment(byteVector, payment)])"
  const invalidInvokeArgBeforeFunc = "addressFromStringValue(dapp2).invoke([AttachedPayment(byteVector, payment)])"
  const invalidFunctionErrorResult = invalidFunctionError("invoke", 4)

    test("RIDE-88. Invoke function should compile for Issue RIDE V5 and more", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("", version)
        for (const [byteVector, data, func] of [[randomByteVectorArrayElement(), randomInt(), invoke], [randomByteVectorArrayElement(), randomInt(), invokeArgBeforeFunc]]) {
          const script = precondition.codeForDAppInvocation(byteVector, data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-89. Invoke function throws an error for invalid values for RIDE V5 and more", () => {
      for (const version of versionsSupportingTheNewFeatures) {
        const precondition = new ContractGenerator("", version)
        for (const [byteVector, data, func, error] of [[randomByteVectorArrayElement(), randomAddressDataArrayElement(), invoke, nonMatchingTypes("Int")], [randomByteVectorArrayElement(), randomDigestAlgorithmTypeArrayElement(), invokeArgBeforeFunc, nonMatchingTypes("Int")], [randomStringArrayElement(), randomInt(), invoke, nonMatchingTypes("ByteVector|Unit")], [randomStringArrayElement(), randomInt(), invokeArgBeforeFunc, nonMatchingTypes("ByteVector|Unit")], [randomByteVectorArrayElement(), randomInt(), invalidInvokeFunction, invalidFunctionErrorResult], [randomByteVectorArrayElement(), randomInt(), invalidInvokeArgBeforeFunc, invalidFunctionErrorResult]]) {
          const script = precondition.codeForDAppInvocation(byteVector, data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
