import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("Take", () => {
  const take = `take(callerTestData, ${randomInt()})`
  const takeArgBeforeFunction = `callerTestData.take(${randomInt()})`
  const invalidTake = `take(callerTestData)`
  const invalidTakeArgBeforeFunc = `callerTestData.take(callerTestData, ${randomInt()})`
  const invalidTakeNotInt = `take(callerTestData, ${randomByteVectorArrayElement()})`

    test("RIDE-56. Take function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), take], [randomByteVectorArrayElement(), takeArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-57. Take function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), take], [randomUnionArrayElement(), takeArgBeforeFunction], [randomByteVectorArrayElement(), invalidTake], [randomByteVectorArrayElement(), invalidTakeArgBeforeFunc], [randomByteVectorArrayElement(), invalidTakeNotInt]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
