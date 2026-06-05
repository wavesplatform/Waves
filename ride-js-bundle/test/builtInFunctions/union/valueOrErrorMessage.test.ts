import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomInt, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { MATCHING_NOT_EXHAUSTIVE, actualVersions, invalidFunctionError } from "../../helpers/testData";

describe("ValueOrErrorMessage", () => {
  const valueOrErrorMessage = `valueOrErrorMessage(callerTestData, \"error message\")`
  const valueOrErrorMessageArgBeforeFunction = `callerTestData.valueOrErrorMessage(\"error message\")`
  const invalidValueOrErrorMessage = `valueOrErrorMessage(callerTestData)`
  const invalidValueOrErrorMessageArgBeforeFunc = `callerTestData.valueOrErrorMessage(callerTestData, \"error message\")`
  const valueOrErrorInvalidFunctionMessage = invalidFunctionError("valueOrErrorMessage", 2)

    test("RIDE-235. valueOrErrorMessage functions are compiled with valid data types.", () => {
      for (const version of actualVersions) {
        for (const [dataType, firstData, func] of [["Int", randomInt(), valueOrErrorMessage], ["Boolean", randomBoolean(), valueOrErrorMessage], ["String", randomStringArrayElement(), valueOrErrorMessage], ["ByteVector", randomByteVectorArrayElement(), valueOrErrorMessageArgBeforeFunction], ["Address", randomAddressDataArrayElement(), valueOrErrorMessageArgBeforeFunction], ["Alias", randomAliasDataArrayElement(), valueOrErrorMessageArgBeforeFunction]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(firstData, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-236. valueOrErrorMessage function should throw a compilation error for invalid data.", () => {
      for (const version of actualVersions) {
        for (const [dataType, firstData, func, error] of [["Alias", randomAddressDataArrayElement(), valueOrErrorMessage, MATCHING_NOT_EXHAUSTIVE], ["String", randomUnionArrayElement(), valueOrErrorMessageArgBeforeFunction, MATCHING_NOT_EXHAUSTIVE], ["String", randomStringArrayElement(), invalidValueOrErrorMessage, valueOrErrorInvalidFunctionMessage], ["ByteVector", randomByteVectorArrayElement(), invalidValueOrErrorMessageArgBeforeFunc, valueOrErrorInvalidFunctionMessage]]) {
          const precondition = new ContractGenerator(dataType, version)
          const script = precondition.onlyMatcherContract(firstData, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
