import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("CreateMerkleRoot", () => {
  const createMerkleRoot = `createMerkleRoot([callerTestData], callerTestData, ${randomInt()})`
  const createMerkleRootArgBeforeFunc = `[callerTestData].createMerkleRoot(callerTestData, ${randomInt()})`
  const invalidCreateMerkleRoot = "createMerkleRoot()"
  const invalidCreateMerkleRootArgBeforeFunc = "[callerTestData].createMerkleRoot(callerTestData)"
  const invalidErrorCreateMerkleRoot = invalidFunctionError("createMerkleRoot", 3)

    test("RIDE-271. createMerkleRoot function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), createMerkleRoot], [randomByteVectorArrayElement(), createMerkleRootArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-272. createMerkleRoot function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), createMerkleRoot, nonMatchingTypes("List[ByteVector]")], [randomAddressDataArrayElement(), createMerkleRootArgBeforeFunc, nonMatchingTypes("List[ByteVector]")], [randomByteVectorArrayElement(), invalidCreateMerkleRoot, invalidErrorCreateMerkleRoot], [randomByteVectorArrayElement(), invalidCreateMerkleRootArgBeforeFunc, invalidErrorCreateMerkleRoot]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-273. Can't find a function createMerkleRoot", () => {
      const precondition = new ContractGenerator("ByteVector", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), createMerkleRoot)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
