import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("TakeRight", () => {
  const takeRight = `takeRight(callerTestData, ${randomInt()})`
  const takeRightArgBeforeFunction = `callerTestData.takeRight(${randomInt()})`
  const invalidTakeRight = `takeRight(callerTestData)`
  const invalidTakeRightArgBeforeFunc = `callerTestData.takeRight(callerTestData, ${randomInt()})`
  const invalidTakeRightNotInt = `takeRight(callerTestData, ${randomByteVectorArrayElement()})`

    test("RIDE-58. TakeRight function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), takeRight], [randomByteVectorArrayElement(), takeRightArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-59. TakeRight function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), takeRight], [randomUnionArrayElement(), takeRightArgBeforeFunction], [randomByteVectorArrayElement(), invalidTakeRight], [randomByteVectorArrayElement(), invalidTakeRightArgBeforeFunc], [randomByteVectorArrayElement(), invalidTakeRightNotInt]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
