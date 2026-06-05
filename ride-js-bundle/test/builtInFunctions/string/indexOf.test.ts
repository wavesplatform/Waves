import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_A_FUNCTION_OVERLOAD, actualVersions, intList, stringList } from "../../helpers/testData";

describe("IndexOf", () => {
  const indexOf = "indexOf(bar, foo)"
  const indexOfWithOffset = `indexOf(bar, foo, ${randomInt()})`
  const indexOfArgBeforeFunc = "bar.indexOf(foo)"
  const indexOfWithOffsetArgBeforeFunc = `bar.indexOf(foo, ${randomInt()})`
  const invalidIndexOf = "indexOf()"

    test("RIDE-215. function indexOf should compile for valid data", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [firstData, secondData, func] of [[randomStringArrayElement(), randomStringArrayElement(), indexOf], [randomStringArrayElement(), randomStringArrayElement(), indexOfWithOffset], [randomStringArrayElement(), randomStringArrayElement(), indexOfArgBeforeFunc], [randomStringArrayElement(), randomStringArrayElement(), indexOfWithOffsetArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(firstData, secondData, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-216. function indexOf - Non-matching types - Can't find a function overload", () => {
      for (const version of actualVersions) {
        const precondition = new ContractGenerator("", version)
        for (const [firstData, secondData, func] of [[stringList, randomStringArrayElement(), indexOf], [randomInt(), randomStringArrayElement(), indexOfArgBeforeFunc], [stringList, randomStringArrayElement(), indexOfWithOffset], [randomInt(), randomStringArrayElement(), indexOfWithOffsetArgBeforeFunc], [randomInt(), intList, invalidIndexOf]]) {
          const script = precondition.simpleRideCode(firstData, secondData, func)
          assertCompileErrorDApp(script, version, CANT_FIND_A_FUNCTION_OVERLOAD)
        }
      }
  });
});
