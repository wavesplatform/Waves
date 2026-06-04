import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, intList } from "../../helpers/testData";

describe("Size", () => {
  const size = "size(callerTestData)"
  const sizeArgBeforeFunc = "callerTestData.size()"
  const invalidSize = "size()"
  const invalidSizeArgBeforeFunc = "callerTestData.size(callerTestData)"

    test("RIDE-170. Function Size should compile for valid list", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[intList, size], [intList, sizeArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-171. Function Size should throw an error for invalid data or type", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomUnionArrayElement(), size], [randomAddressDataArrayElement(), sizeArgBeforeFunc], [intList, invalidSize], [intList, invalidSizeArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
