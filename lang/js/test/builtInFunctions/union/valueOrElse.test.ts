import { describe, test } from "vitest";
import { assertCompileErrorDApp, assertCompileSuccessDApp, V3 } from "../../helpers/jsTestBase";
import { ContractGenerator } from "../../helpers/contractGenerator";
import { randomAddressDataArrayElement, randomAliasDataArrayElement, randomBoolean, randomByteVectorArrayElement, randomInt, randomStringArrayElement } from "../../helpers/randomData";
import { CANT_FIND_FUNCTION, CANT_MATCH_INFERRED_TYPE, actualVersionsWithoutV3, invalidFunctionError } from "../../helpers/testData";

describe("ValueOrElse", () => {
  const valueOrElse = "valueOrElse(bar, foo)"
  const valueOrElseArgBeforeFunc = "bar.valueOrElse(foo)"
  const invalidValueOrElse = "valueOrElse(foo)"
  const invalidValueOrElseArgBeforeFunc = "foo.valueOrElse(foo, bar)"
  const invalidErrorValueOrElse = invalidFunctionError("valueOrElse", 2)

    test("RIDE-232. function valueOrElse should compile for valid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [firstData, secondData, func] of [[randomStringArrayElement(), randomStringArrayElement(), valueOrElse], [randomInt(), randomInt(), valueOrElse], [randomAliasDataArrayElement(), randomAliasDataArrayElement(), valueOrElse], [randomAddressDataArrayElement(), randomAddressDataArrayElement(), valueOrElseArgBeforeFunc], [randomByteVectorArrayElement(), randomByteVectorArrayElement(), valueOrElseArgBeforeFunc], [randomBoolean(), randomBoolean(), valueOrElseArgBeforeFunc]]) {
          const script = precondition.simpleRideCode(firstData, secondData, func)
          assertCompileSuccessDApp(script, version)
        }
      }
  });

    test("RIDE-233. function valueOrElse throw a compilation error for invalid data", () => {
      for (const version of actualVersionsWithoutV3) {
        const precondition = new ContractGenerator("", version)
        for (const [firstData, secondData, func, error] of [[randomInt(), randomByteVectorArrayElement(), valueOrElse, CANT_MATCH_INFERRED_TYPE], [randomInt(), randomStringArrayElement(), valueOrElseArgBeforeFunc, CANT_MATCH_INFERRED_TYPE], [randomStringArrayElement(), randomStringArrayElement(), invalidValueOrElse, invalidErrorValueOrElse], [randomByteVectorArrayElement(), randomByteVectorArrayElement(), invalidValueOrElseArgBeforeFunc, invalidErrorValueOrElse]]) {
          const script = precondition.simpleRideCode(firstData, secondData, func)
          assertCompileErrorDApp(script, version, error)
        }
      }
  });

    test("RIDE-234. Can't find a function valueOrElse for RIDE V3", () => {
      const precondition = new ContractGenerator("", V3)
      for (const [firstData, secondData, func] of [[randomAliasDataArrayElement(), randomAliasDataArrayElement(), valueOrElse], [randomAddressDataArrayElement(), randomAddressDataArrayElement(), valueOrElseArgBeforeFunc]]) {
        const script = precondition.simpleRideCode(firstData, secondData, func)
        assertCompileErrorDApp(script, V3, CANT_FIND_FUNCTION)
      }
  });
});
