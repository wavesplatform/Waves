// The other suites load dist/index.js from the working tree. These check what consumers actually get: the tarball
// `npm publish` would upload (the `files` list, `main`, `types`, declared dependencies) and the webpack browser bundle.
import { describe, test, expect, beforeAll, afterAll } from "vitest";
import { execFileSync } from "child_process";
import { createRequire, isBuiltin } from "module";
import * as fs from "fs";
import * as path from "path";
import * as vm from "vm";
import { fileURLToPath } from "url";
import { ride } from "./helpers/compiler";
import { dApp } from "./helpers/jsTestBase";

const root = fileURLToPath(new URL("..", import.meta.url));
const require = createRequire(import.meta.url);

// A dApp that goes through the crypto globals too (the checksum in base64 comes from @waves/ts-lib-crypto).
const contract = dApp(
  `
@Callable(i)
func c() = [IntegerEntry("k", size(toBase58String(blake2b256(i.caller.bytes))))]
@Verifier(tx)
func v() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)`,
  6
);

describe("packed tarball", () => {
  let dir: string;
  let pkgDir: string;
  let pkg: any;

  beforeAll(() => {
    // Under node_modules so the unpacked package resolves its dependencies from this package's node_modules.
    const cache = path.join(root, "node_modules", ".cache");
    fs.mkdirSync(cache, { recursive: true });
    dir = fs.mkdtempSync(path.join(cache, "ride-js-pack-"));
    const out = JSON.parse(
      execFileSync("npm", ["pack", "--json", "--ignore-scripts", "--pack-destination", dir], { cwd: root }).toString()
    );
    // npm 11 keys the result by package name; older npm returns an array.
    const { filename } = Array.isArray(out) ? out[0] : Object.values<any>(out)[0];
    execFileSync("tar", ["-xzf", path.join(dir, filename), "-C", dir]);
    pkgDir = path.join(dir, "package");
    pkg = JSON.parse(fs.readFileSync(path.join(pkgDir, "package.json"), "utf8"));
  });

  afterAll(() => {
    if (dir) fs.rmSync(dir, { recursive: true, force: true });
  });

  test("ships the main entry and the typings", () => {
    expect(fs.existsSync(path.join(pkgDir, pkg.main))).toBe(true);
    expect(fs.existsSync(path.join(pkgDir, pkg.types))).toBe(true);
  });

  test("compiles through the main entry like the working tree", () => {
    const packed = require(path.join(pkgDir, pkg.main));
    expect(Object.keys(packed).sort()).toEqual(Object.keys(ride).sort());
    expect(packed.compile(contract).result.base64).toEqual(ride.compile(contract).result.base64);
  });

  test("every package it requires at runtime is a declared dependency", () => {
    const files = ["dist", "scalajs"]
      .flatMap((d) => fs.readdirSync(path.join(pkgDir, d)).map((f) => path.join(d, f)))
      // ride.min.js is self-contained: webpack inlined its dependencies.
      .filter((f) => f.endsWith(".js") && !f.endsWith("ride.min.js"));
    const required = new Set(
      files.flatMap((f) =>
        [...fs.readFileSync(path.join(pkgDir, f), "utf8").matchAll(/require\("([^".][^"]*)"\)/g)].map((m) => m[1])
      )
    );
    const undeclared = [...required].filter((r) => !isBuiltin(r) && !(r in (pkg.dependencies ?? {})));
    expect(required.size).toBeGreaterThan(0);
    expect(undeclared).toEqual([]);
  });
});

describe("browser bundle", () => {
  test("runs without Node globals and compiles like the node entry", () => {
    // Only what a browser has: no require, process, Buffer or module.
    const g: any = { console, TextEncoder, TextDecoder, crypto: globalThis.crypto, setTimeout, clearTimeout, URL };
    g.self = g.window = g.globalThis = g;
    vm.runInContext(fs.readFileSync(path.join(root, "dist", "ride.min.js"), "utf8"), vm.createContext(g), {
      filename: "ride.min.js"
    });
    const bundle = g.RideJS;
    expect(Object.keys(bundle).sort()).toEqual(Object.keys(ride).sort());
    expect(bundle.compile(contract).result.base64).toEqual(ride.compile(contract).result.base64);
  });
});
