// Sets the crypto/http primitives the ScalaJS compiler & repl resolve from the
// global scope. Ported from the original @waves/ride-js src/interop.js. Must run
// before the ScalaJS artifact is required.
const crypto: any = require("@waves/ts-lib-crypto");
const axios: any = require("axios");

const g = global as any;

g.base58Encode = function (bytes: ArrayBuffer | Uint8Array | number[]) {
  return crypto.base58Encode(new Uint8Array(bytes as any));
};
g.base58Decode = function (data: string) {
  return crypto.base58Decode(data).buffer;
};
g.base64Encode = function (bytes: ArrayBuffer | Uint8Array | number[]) {
  return crypto.base64Encode(new Uint8Array(bytes as any));
};
g.base64Decode = function (data: string) {
  return crypto.base64Decode(data);
};
g.keccak256 = function (bytes: ArrayBuffer | Uint8Array | number[]) {
  return Uint8Array.from(crypto.keccak(new Uint8Array(bytes as any))).buffer;
};
g.sha256 = function (bytes: ArrayBuffer | Uint8Array | number[]) {
  return Buffer.from(crypto.sha256(new Uint8Array(bytes as any)), "hex");
};
g.blake2b256 = function (bytes: ArrayBuffer | Uint8Array | number[]) {
  return crypto.blake2b(new Uint8Array(bytes as any)).buffer;
};
g.curve25519verify = function (msg: ArrayBuffer, sig: ArrayBuffer, key: ArrayBuffer) {
  return crypto.verifySignature(new Uint8Array(key), new Uint8Array(msg), new Uint8Array(sig));
};
g.merkleVerify = function (rootHash: ArrayBuffer, merkleProof: ArrayBuffer, leafData: ArrayBuffer) {
  return crypto.merkleVerify(new Uint8Array(rootHash), new Uint8Array(merkleProof), new Uint8Array(leafData));
};
// RIDE digest names that @waves/ts-lib-crypto spells differently; NONE means no digest. Others pass through.
const RSA_ALG: Record<string, string | undefined> = {
  SHA3224: "SHA3-224",
  SHA3256: "SHA3-256",
  SHA3384: "SHA3-384",
  SHA3512: "SHA3-512",
  NONE: undefined
};
g.rsaVerify = function (digest: any, msg: ArrayBuffer, sig: ArrayBuffer, key: ArrayBuffer) {
  const name = String(digest);
  const alg = name in RSA_ALG ? RSA_ALG[name] : name;
  return crypto.rsaVerify(new Uint8Array(key), new Uint8Array(msg), new Uint8Array(sig), alg);
};
// The repl fetches from the configured nodeUrl, which must be trusted: only http(s), no redirects, bounded in time
// and size, so a hostile or broken URL cannot hang the caller or exhaust its memory.
g.httpGet = async function (data: any) {
  if (!data.url) return { ...data, status: 404, body: "url is undefined" };
  let protocol: string;
  try {
    protocol = new URL(data.url).protocol;
  } catch {
    return { ...data, status: 400, body: `invalid url: ${data.url}` };
  }
  if (protocol !== "http:" && protocol !== "https:")
    return { ...data, status: 400, body: `unsupported protocol: ${protocol}` };
  const resp = await axios.get(data.url, {
    validateStatus: () => true,
    timeout: 30_000,
    maxContentLength: 10 * 1024 * 1024,
    maxRedirects: 0
  });
  const status = resp.status;
  let body = resp.data;
  if (typeof body !== "string") body = JSON.stringify(body);
  return { ...data, status, body };
};
export {};
