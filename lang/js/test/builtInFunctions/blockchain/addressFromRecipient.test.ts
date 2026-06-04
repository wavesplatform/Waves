import { describe, test } from "vitest";
import { assertCompileErrorExpression, assertCompileSuccessExpression } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomInt } from "../../helpers/randomData";
import { actualVersions, invalidFunctionError, nonMatchingTypes, thisVariable } from "../../helpers/testData";

describe("AddressFromRecipient", () => {
  const addressFromRecipient = "addressFromRecipient(addressOrAlias)"
  const addressFromRecipientArgBeforeFunc = "addressOrAlias.addressFromRecipient()"
  const invalidFunc = "addressFromRecipient()"

    test("RIDE-24. Compile addressFromRecipient function for address, alias, and 'this'", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [addressOrAlias, func, address] of [[randomAddressDataArrayElement(), addressFromRecipient, randomAddressDataArrayElement()], [randomAliasDataArrayElement(), addressFromRecipient, randomAddressDataArrayElement()], [thisVariable, addressFromRecipient, randomAddressDataArrayElement()], [randomAddressDataArrayElement(), addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement()], [randomAliasDataArrayElement(), addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement()], [thisVariable, addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement()]]) {
          const script = precondition.codeForAddressFromRecipient(addressOrAlias, func, address)
          assertCompileSuccessExpression(script, version)
        }
      }
  });

    test("RIDE-25. Invalid data for functions addressFromRecipient", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [addressOrAlias, func, address] of [[randomInt(), addressFromRecipient, randomAddressDataArrayElement()], [randomInt(), addressFromRecipientArgBeforeFunc, randomAddressDataArrayElement()]]) {
          const script = precondition.codeForAddressFromRecipient(addressOrAlias, func, address)
          assertCompileErrorExpression(script, version, nonMatchingTypes("Address|Alias"))
        }
      }
  });

    test("RIDE-26. Function 'addressFromRecipient' requires 1 arguments", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        const script = precondition.codeForAddressFromRecipient(
          randomAddressDataArrayElement(),
          invalidFunc,
          randomAddressDataArrayElement()
        )
        assertCompileErrorExpression(script, version, invalidFunctionError("addressFromRecipient", 1))
      }
  });
});
