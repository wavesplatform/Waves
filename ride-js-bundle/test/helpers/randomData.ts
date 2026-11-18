// Port of testHelpers/RandomDataGenerator.scala
// Picks random RIDE source fragments from fixed fixtures. As in the original,
// randomness only selects which valid/invalid fixture is fed to the compiler;
// assertions are about compile success/error, not the concrete value.
// Seeded from RIDE_TEST_SEED (set and printed by test/globalSetup.ts), so a failure reproduces with the same seed.

function mulberry32(seed: number): () => number {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

const random = mulberry32(Number(process.env.RIDE_TEST_SEED ?? Math.floor(Math.random() * 2 ** 32)));

function pick(arr: readonly string[]): string {
  return arr[Math.floor(random() * arr.length)];
}

export const addressDataArray: readonly string[] = [
  "Address(base58'')",
  "Address(base58'3MDaMwqLtwBGcJrTA5tstJfY95GqnNnDDAS')",
  "Address(base58'3PDaScqLtwBGcJrTA5tstJfY95GqnNnLxGA')",
  "Address(base58'3P3aScAJsxBGcJrTA5tstJfY95GqnNnHLGA')"
];

export const aliasDataArray: readonly string[] = [
  'Alias("merry_1312@pro")',
  'Alias("four")',
  'Alias("1111this_alias_30@long-symbols")',
  'Alias("")'
];

export const byteVectorArray: readonly string[] = [
  "base16'52696465'",
  "base58'8t38fWQhrYJsqxXtPpiRCEk1g5RJdq9bG5Rkr2N7mDFC'",
  "base64'UmlkZQ=='"
];

export const stringArray: readonly string[] = [
  '"3P3aScAJsxBGcJrTA5tstJfY95GqnNnHLGAlsadjbf87"',
  '"8t38fWQhrYJsqxXtPpiRCEk1g5RJdq9bG5Rkr2N7mDFC"',
  '"3MDaMwqLtwBGcJrTA5tstJfY95GqnNnDDASmsakmd091"',
  '"3P3aScAJsxBGGqnNnHLGAlsadjbf87"',
  '"8t38fWQhrYJsqxXtPpiRasdiuh32he98ddr2N7"',
  '"091"'
];

export const issuesArray: readonly string[] = [
  'Issue("superToken", "token for super humans", 1000, 2, true)',
  'Issue("trueToken", "real token in real life", 5_000_000_000, 5, false)',
  'Issue("oneMoreToken", "just one more token in this crypto world", 1_500_000_000, 5, true)',
  'Issue("assetForAll", "asset for all peoples", 1_500_000_000, 5, true)'
];

export const unionArray: readonly string[] = ["DOWN", "CEILING", "FLOOR", "HALFUP", "HALFEVEN"];

export const digestAlgorithmTypeArray: readonly string[] = [
  "NOALG",
  "MD5",
  "SHA1",
  "SHA224",
  "SHA256",
  "SHA384",
  "SHA512",
  "SHA3224",
  "SHA3256",
  "SHA3384",
  "SHA3512"
];

// Like the other generators, these return RIDE source literals, not JS values.
export function randomInt(): string {
  return String(Math.floor(random() * 90000000));
}

export function randomBoolean(): string {
  return String(random() < 0.5);
}

export const randomAddressDataArrayElement = (): string => pick(addressDataArray);
export const randomAliasDataArrayElement = (): string => pick(aliasDataArray);
export const randomByteVectorArrayElement = (): string => pick(byteVectorArray);
export const randomStringArrayElement = (): string => pick(stringArray);
export const randomIssuesArrayElement = (): string => pick(issuesArray);
export const randomUnionArrayElement = (): string => pick(unionArray);
export const randomDigestAlgorithmTypeArrayElement = (): string => pick(digestAlgorithmTypeArray);

export function entryMap(key: string): string {
  const entryArray: Record<string, string> = {
    BinaryEntry: `[BinaryEntry("key", ${randomByteVectorArrayElement()})]`,
    IntegerEntry: `[IntegerEntry("key", ${randomInt()})]`,
    StringEntry: `[StringEntry("key", ${randomStringArrayElement()})]`,
    BooleanEntry: `[BooleanEntry("key", ${randomBoolean()})]`
  };
  return entryArray[key];
}

export function dataEntryForTests(value: string): string {
  return `[DataEntry("key", ${value})]`;
}
