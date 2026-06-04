// The compiled RIDE compiler (dist/lang.js) expects a set of crypto primitives
// to be available in the global scope (see com.wavesplatform.lang.impl.Global).
// During compilation only the hashing functions are actually invoked (e.g. to
// derive script ids); the original Scala/utest suite injected identity stubs for
// blake2b256/keccak256. We mirror that here and add the remaining primitives so
// any constant-folding path stays safe. Tests only assert compiler output, never
// the cryptographic results, so identity/true stubs are sufficient.

type Buf = ArrayBuffer;

const g = globalThis as unknown as Record<string, unknown>;

const identity = (message: Buf): Buf => message;

g.blake2b256 = identity;
g.keccak256 = identity;
g.sha256 = identity;
g.curve25519verify = (): boolean => true;
g.rsaVerify = (): boolean => true;
g.merkleVerify = (): boolean => true;
