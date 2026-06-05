import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomByteVectorArrayElement, randomInt, randomUnionArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, GreaterV3ResultBinaryEntry, actualVersions, rideV3Result } from "../../helpers/testData";

describe("DropRight", () => {
  const dropRight = `dropRight(callerTestData, ${randomInt()})`
  const dropRightArgBeforeFunction = `callerTestData.dropRight(${randomInt()})`
  const invalidDropRight = `dropRight(callerTestData)`
  const invalidDropRightNotInt = `dropRight(callerTestData, ${randomByteVectorArrayElement()})`
  const invalidDropRightArgBeforeFunc = `callerTestData.dropRight(callerTestData, ${randomInt()})`

    test("RIDE-52. DropRight function should compile for valid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomByteVectorArrayElement(), dropRight], [randomByteVectorArrayElement(), dropRightArgBeforeFunction]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-53. DropRight function throws an error for invalid values", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [data, func] of [[randomAddressDataArrayElement(), dropRight], [randomUnionArrayElement(), dropRightArgBeforeFunction], [randomByteVectorArrayElement(), invalidDropRight], [randomByteVectorArrayElement(), invalidDropRightArgBeforeFunc], [randomByteVectorArrayElement(), invalidDropRightNotInt]]) {
          const script = precondition.codeWithoutMatcher(data, func, rideV3Result, GreaterV3ResultBinaryEntry)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
