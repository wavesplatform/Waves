import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("Bn256Groth16Verify", () => {
  const bn256Groth16Verify = `bn256Groth16Verify(callerTestData, callerTestData, callerTestData)`
  const bn256Groth16VerifyArgBeforeFunc = `callerTestData.bn256Groth16Verify(callerTestData, callerTestData)`
  const invalidBn256Groth16Verify = "bn256Groth16Verify()"
  const invalidBn256Groth16VerifyArgBeforeFunc = "callerTestData.bn256Groth16Verify(callerTestData)"
  const invalidErrorBn256Groth16Verify = invalidFunctionError("bn256Groth16Verify", 3)

    test("RIDE-265. bn256Groth16Verify function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), bn256Groth16Verify], [randomByteVectorArrayElement(), bn256Groth16VerifyArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-266. bn256Groth16Verify function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), bn256Groth16Verify, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), bn256Groth16VerifyArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidBn256Groth16Verify, invalidErrorBn256Groth16Verify], [randomByteVectorArrayElement(), invalidBn256Groth16VerifyArgBeforeFunc, invalidErrorBn256Groth16Verify]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-267. Can't find a function bn256Groth16Verify", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), bn256Groth16Verify)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
