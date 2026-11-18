// Port of testHelpers/TestDataConstantsAndMethods.scala
import { V3, V4, V5, V6 } from "./jsTestBase";
import { entryMap } from "./randomData";

export const oldVersions: number[] = [V3, V4];
export const versionsSupportingTheNewFeatures: number[] = [V5, V6];
export const versionsWithoutV6: number[] = [V3, V4, V5];
export const actualVersionsWithoutV3: number[] = [V4, V5, V6];
export const actualVersions: number[] = [V3, V4, V5, V6];

export const CANT_FIND_A_FUNCTION_OVERLOAD = "Can't find a function overload";
export const CANT_FIND_FUNCTION = "Can't find a function";
export const CANT_MATCH_INFERRED_TYPE = "Can't match inferred types of T over";
export const MATCHING_NOT_EXHAUSTIVE = "Matching not exhaustive:";
export const UNDEFINED_TYPE = "Undefined type: `BigInt`";
export const stringList = '["a", "b", "c", "d", "e", "f", "g"]';
export const intList = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
export const thisVariable = "this";

export function nonMatchingTypes(expectType: string): string {
  return `Non-matching types: expected: ${expectType}`;
}

export function invalidFunctionError(functionName: string, numberOfArguments: number): string {
  return `Function '${functionName}' requires ${numberOfArguments} arguments`;
}

export const binaryEntryForTests: string = entryMap("BinaryEntry");
export const integerEntryForTests: string = entryMap("IntegerEntry");
export const stringEntryForTests: string = entryMap("StringEntry");
export const booleanEntryForTests: string = entryMap("BooleanEntry");

export const rideV3Result = `
WriteSet([
    DataEntry("dataType", val)
  ])
`;

export const GreaterV3ResultBinaryEntry = `
[
    BinaryEntry("bin", val)
]
`;

export const GreaterV3ResultBooleanEntry = `
[
    BooleanEntry("boolean", val)
]
`;

export const GreaterV3ResultIntegerEntry = `
[
    IntegerEntry("integer", val)
]
`;

export const GreaterV3ResultStringEntry = `
[
    StringEntry("String", val)
]
`;
