import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, intList, stringList } from "../../helpers/testData";

describe("LastIndexOf", () => {
  const lastIndexOf = "lastIndexOf(bar, foo)"
  const lastIndexOfWithOffset = `lastIndexOf(bar, foo, ${randomInt()})`
  const lastIndexOfArgBeforeFunc = "bar.lastIndexOf(foo)"
  const lastIndexOfWithOffsetArgBeforeFunc = `bar.lastIndexOf(foo, ${randomInt()})`
  const invalidLastIndexOf = "lastIndexOf()"

    test("RIDE-217. function lastIndexOf should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [firstData, secondData, func] of [[randomStringArrayElement(), randomStringArrayElement(), lastIndexOf], [randomStringArrayElement(), randomStringArrayElement(), lastIndexOfWithOffset], [randomStringArrayElement(), randomStringArrayElement(), lastIndexOfArgBeforeFunc], [randomStringArrayElement(), randomStringArrayElement(), lastIndexOfWithOffsetArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(firstData, secondData, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-218. function lastIndexOf - Non-matching types - Can't find a function overload", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [firstData, secondData, func] of [[stringList, randomStringArrayElement(), lastIndexOf], [randomInt(), randomStringArrayElement(), lastIndexOfArgBeforeFunc], [stringList, randomStringArrayElement(), lastIndexOfWithOffset], [randomInt(), randomStringArrayElement(), lastIndexOfWithOffsetArgBeforeFunc], [randomInt(), intList, invalidLastIndexOf]]) {
          const script = precondition.simpleRideCode(firstData, secondData, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
