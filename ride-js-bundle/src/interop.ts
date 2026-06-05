// Sets the crypto/http primitives the ScalaJS compiler & repl resolve from the
// global scope. Ported from the original @waves/ride-js src/interop.js. Must run
// before the ScalaJS artifact is required.
/* eslint-disable @typescript-eslint/no-explicit-any */
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
g.rsaVerify = function (digest: any, msg: ArrayBuffer, sig: ArrayBuffer, key: ArrayBuffer) {
  let alg = digest.toString();
  switch (digest.toString()) {
    case "SHA3224":
      alg = "SHA3-224";
      break;
    case "SHA3256":
      alg = "SHA3-256";
      break;
    case "SHA3384":
      alg = "SHA3-384";
      break;
    case "SHA3512":
      alg = "SHA3-512";
      break;
    case "NONE":
      alg = undefined;
      break;
  } // fixme
  return crypto.rsaVerify(new Uint8Array(key), new Uint8Array(msg), new Uint8Array(sig), alg);
};
g.httpGet = async function (data: any) {
  if (!data.url) return { ...data, status: 404, body: "url is undefined" };
  const resp = await axios.get(data.url, { validateStatus: () => true });
  const status = resp.status;
  let body = await resp.data;
  if (typeof body !== "string") body = JSON.stringify(body);
  return { ...data, status, body };
};
export {};
