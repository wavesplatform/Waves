import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, intList, stringList } from "../../helpers/testData";

describe("IndexOf", () => {
  const indexOf = "indexOf(bar, foo)"
  const indexOfArgBeforeFunc = "bar.indexOf(foo)"
  const invalidIndexOf = "indexOf()"
  const invalidIndexOfArgBeforeFunc = "bar.indexOf(bar, foo)"

    test("RIDE-158. Function IndexOf should compile for valid list", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, indexOf], [randomInt(), intList, indexOf], [randomStringArrayElement(), stringList, indexOfArgBeforeFunc], [randomInt(), intList, indexOfArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-159. Function IndexOf should throw an error for invalid data or type", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[stringList, stringList, indexOf], [randomByteVectorArrayElement(), intList, indexOfArgBeforeFunc], [randomInt(), intList, invalidIndexOf], [randomStringArrayElement(), stringList, invalidIndexOfArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
