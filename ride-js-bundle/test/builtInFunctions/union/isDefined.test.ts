import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomDigestAlgorithmTypeArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { MATCHING_NOT_EXHAUSTIVE, actualVersions, invalidFunctionError } from "../../helpers/testData";

describe("IsDefined", () => {
  const isDefined = "isDefined(callerTestData)"
  const isDefinedArgBeforeFunc = "callerTestData.isDefined()"
  const invalidIsDefined = "isDefined()"
  const invalidIsDefinedArgBeforeFunc = "callerTestData.isDefined(callerTestData)"
  const invalidErrorIsDefined = invalidFunctionError("isDefined", 1)

    test("RIDE-228. function isDefined should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), isDefined], [randomAliasDataArrayElement(), isDefinedArgBeforeFunc], [randomByteVectorArrayElement(), isDefined], [randomStringArrayElement(), isDefinedArgBeforeFunc], [randomUnionArrayElement(), isDefined], [randomDigestAlgorithmTypeArrayElement(), isDefinedArgBeforeFunc], [randomInt(), isDefined], [randomBoolean(), isDefinedArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-229. function isDefined throw a compilation error for can't find overload", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Boolean", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), invalidIsDefined, MATCHING_NOT_EXHAUSTIVE], [randomAddressDataArrayElement(), invalidIsDefinedArgBeforeFunc, MATCHING_NOT_EXHAUSTIVE], [randomInt(), invalidIsDefined, invalidErrorIsDefined], [randomInt(), invalidIsDefinedArgBeforeFunc, invalidErrorIsDefined]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
