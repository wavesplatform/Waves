import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, actualVersionsWithoutV3, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("Ecrecover", () => {
  const ecrecover = "ecrecover(callerTestData, callerTestData)"
  const ecrecoverArgBeforeFunc = "callerTestData.ecrecover(callerTestData)"
  const invalidEcrecover = "ecrecover()"
  const invalidEcrecoverArgBeforeFunc = "callerTestData.ecrecover()"
  const invalidErrorEcrecover = invalidFunctionError("ecrecover", 2)

    test("RIDE-274. ecrecover function should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), ecrecover], [randomByteVectorArrayElement(), ecrecoverArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-275. ecrecover function should throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("ByteVector", version)
        for (const [data, func, error] of [[randomUnionArrayElement(), ecrecover, nonMatchingTypes("ByteVector")], [randomAddressDataArrayElement(), ecrecoverArgBeforeFunc, nonMatchingTypes("ByteVector")], [randomByteVectorArrayElement(), invalidEcrecover, invalidErrorEcrecover], [randomByteVectorArrayElement(), invalidEcrecoverArgBeforeFunc, invalidErrorEcrecover]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-276. Can't find a function ecrecover", () => {
      const precondition = new ContractGenerator("ByteVector", V3)
      const script = precondition.onlyMatcherContract(randomByteVectorArrayElement(), ecrecover)
      assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
  });
});
