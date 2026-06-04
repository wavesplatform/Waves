import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAliasDataArrayElement, randomBoolean, randomStringArrayElement, randomUnionArrayElement } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes } from "../../helpers/testData";

describe("AddressFromString", () => {
  const addressFromString = "addressFromString(callerTestData)"
  const addressFromStringArgBeforeFunc = "callerTestData.addressFromString()"
  const addressFromStringValue = "addressFromStringValue(callerTestData)"
  const addressFromStringValueArgBeforeFunc = "callerTestData.addressFromStringValue()"
  const invalidAddressFromString = "addressFromString()"
  const invalidAddressFromStringValue = "addressFromStringValue()"
  const invalidFunctionErrorForAddressFromString = invalidFunctionError("addressFromString", 1)
  const invalidFunctionErrorForAddressFromStringValue = invalidFunctionError("addressFromStringValue", 1)

    test("RIDE-116. Function addressFromString should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Address", version)
        for (const [data, func] of [[randomStringArrayElement(), addressFromString], [randomStringArrayElement(), addressFromStringArgBeforeFunc], [randomStringArrayElement(), addressFromStringValue], [randomStringArrayElement(), addressFromStringValueArgBeforeFunc]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-117. Function addressFromString should throw an error for invalid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("Address", version)
        for (const [data, func, error] of [[randomAliasDataArrayElement(), addressFromString, nonMatchingTypes("String")], [randomAliasDataArrayElement(), addressFromStringArgBeforeFunc, nonMatchingTypes("String")], [randomBoolean(), addressFromStringValue, nonMatchingTypes("String")], [randomUnionArrayElement(), addressFromStringValueArgBeforeFunc, nonMatchingTypes("String")], [randomStringArrayElement(), invalidAddressFromString, invalidFunctionErrorForAddressFromString], [randomStringArrayElement(), invalidAddressFromStringValue, invalidFunctionErrorForAddressFromStringValue]]) {
          const script = precondition.onlyMatcherContract(data, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });
});
