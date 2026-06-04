import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3, V4, V6 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, MATCHING_NOT_EXHAUSTIVE, actualVersionsWithoutV3, invalidFunctionError } from "../../helpers/testData";

describe("Extract", () => {
  const extract = "extract(callerTestData)"
  const extractArgBeforeFunc = "callerTestData.extract()"
  const invalidExtract = "extract()"
  const invalidExtractArgBeforeFunc = "callerTestData.extract(callerTestData)"

  const invalidErrorExtract = invalidFunctionError("extract", 1)

    test("RIDE-225. function extract should compile for valid data", () => {
      const precondition = new ContractGenerator("Int", V3)
      for (const [data, func] of [[randomInt(), extract], [randomInt(), extractArgBeforeFunc]]) {
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileSuccessDApp(script, V3)
      }
  });

    test("RIDE-226. function extract throw a compilation error for invalid data", () => {
      const precondition = new ContractGenerator("Int", V3)
      for (const [data, func, error] of [[randomUnionArrayElement(), extract, MATCHING_NOT_EXHAUSTIVE], [randomUnionArrayElement(), extractArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE], [randomInt(), invalidExtract, invalidErrorExtract], [randomInt(), invalidExtractArgBeforeFunc, invalidErrorExtract]]) {
        const script = precondition.onlyMatcherContract(data, func)
        assertCompileErrorDApp(script, V3, error)
      }
  });

    test("RIDE-227. invalid extract functions for RIDE V4 - V6", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("Int", version)
        for (const [data, func] of [[randomInt(), extract], [randomInt(), extractArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, CANT_FIND_FUNCTION)
        }
      }
  });
});
