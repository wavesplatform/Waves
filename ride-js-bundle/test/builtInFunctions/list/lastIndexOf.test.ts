import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomByteVectorArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersionsWithoutV3, intList, stringList } from "../../helpers/testData";

describe("LastIndexOf", () => {
  const lastIndexOf = "lastIndexOf(bar, foo)"
  const lastIndexOfArgBeforeFunc = "bar.lastIndexOf(foo)"
  const invalidLastIndexOf = "lastIndexOf()"
  const invalidLastIndexOfArgBeforeFunc = "bar.indexOf(bar, foo)"

    test("RIDE-160. Function LastIndexOf should compile for valid list", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[randomStringArrayElement(), stringList, lastIndexOf], [randomInt(), intList, lastIndexOf], [randomStringArrayElement(), stringList, lastIndexOfArgBeforeFunc], [randomInt(), intList, lastIndexOfArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-161. Function IndexOf should throw an error for invalid data or type", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [data, list, func] of [[stringList, stringList, lastIndexOf], [randomByteVectorArrayElement(), intList, lastIndexOfArgBeforeFunc], [randomInt(), intList, invalidLastIndexOf], [randomStringArrayElement(), stringList, invalidLastIndexOfArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(data, list, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
