import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, MATCHING_NOT_EXHAUSTIVE, actualVersionsWithoutV3, invalidFunctionError } from "../../helpers/testData";

describe("CheckMerkleProof", () => {
  const checkMerkleProof = "checkMerkleProof(callerTestData, callerTestData, callerTestData)"
  const checkMerkleProofArgBeforeFunc = "callerTestData.checkMerkleProof(callerTestData, callerTestData)"
  const invalidCheckMerkleProof = "checkMerkleProof()"
  const invalidCheckMerkleProofArgBeforeFunc = "callerTestData.checkMerkleProof(callerTestData)"

  const invalidErrorCheckMerkleProof = invalidFunctionError("checkMerkleProof", 3)

    test("RIDE-268. checkMerkleProof function should compile for valid data", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      for (const [data, func] of [[randomByteVectorArrayElement(), checkMerkleProof], [randomByteVectorArrayElement(), checkMerkleProofArgBeforeFunc]]) {
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-269. checkMerkleProof function should throw a compilation error for invalid data", () => {
      const precondition = new ContractGenerator("Boolean", V3)
      for (const [data, func, error] of [[randomUnionArrayElement(), checkMerkleProof, MATCHING_NOT_EXHAUSTIVE], [randomAddressDataArrayElement(), checkMerkleProofArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE], [randomByteVectorArrayElement(), invalidCheckMerkleProof, invalidErrorCheckMerkleProof], [randomByteVectorArrayElement(), invalidCheckMerkleProofArgBeforeFunc, invalidErrorCheckMerkleProof]]) {
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileErrorDApp(script, V3, error)
      }
  });

    test("RIDE-270. Can't find a function checkMerkleProof", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), checkMerkleProof], [randomByteVectorArrayElement(), checkMerkleProofArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
