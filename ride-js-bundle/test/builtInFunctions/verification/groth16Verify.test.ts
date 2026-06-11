import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("Groth16Verify", () => {
  const groth16Verify = "groth16Verify(callerTestData, callerTestData, callerTestData)"
  const groth16VerifyArgBeforeFunc = "callerTestData.groth16Verify(callerTestData, callerTestData)"
  const invalidGroth16Verify = "groth16Verify()"
  const invalidGroth16VerifyArgBeforeFunc = "callerTestData.groth16Verify()"
  const invalidErrorGroth16Verify = invalidFunctionError("groth16Verify", 3)

    test("RIDE-277. groth16Verify function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), groth16Verify], [randomByteVectorArrayElement(), groth16VerifyArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-278. groth16Verify function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), groth16Verify, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), groth16VerifyArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidGroth16Verify, invalidErrorGroth16Verify], [randomByteVectorArrayElement(), invalidGroth16VerifyArgBeforeFunc, invalidErrorGroth16Verify]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-279. Can't find a function groth16Verify", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), groth16Verify)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
