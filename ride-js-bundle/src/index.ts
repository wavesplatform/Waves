// Public @waves/ride-js API, ported from the wavesplatform/ride-js src/index.js. The original required
// @waves/ride-lang and @waves/ride-repl, each with its own copy of the ScalaJS runtime; this requires one
// combined artifact (scalajs/ride-scalajs.js, the linked repl-js output) that exports both APIs.
require("./interop"); // side effect: installs crypto/http globals before the artifact runs
const crypto: any = require("@waves/ts-lib-crypto");
// Exposes both the compiler exports (lang-js JsAPI) and the repl exports (repl-js JsAPI) as one module.
const scalajs: any = require("../scalajs/ride-scalajs.js");

function wrappedCompile(
  code: string,
  estimatorVersion = 3,
  needCompaction = false,
  removeUnusedCode = false,
  libraries: Record<string, string> = {}
): any {
  if (typeof code !== "string") {
    return {
      error: "Type error: contract should be string"
    };
  }
  try {
    const result = scalajs.compile(code, estimatorVersion, needCompaction, removeUnusedCode, libraries);
    if (result.error) {
      try {
        result.size = new Uint8Array(result.result).length;
      } catch (e) {
        // ignore: failed compilations may not carry bytes
      }
      return result;
    } else {
      const bytes = new Uint8Array(result.result);
      const {
        ast,
        complexity,
        verifierComplexity,
        callableComplexities,
        userFunctionComplexities,
        globalVariableComplexities
      } = result;
      return {
        result: {
          bytes,
          base64: crypto.base64Encode(bytes),
          size: bytes.byteLength,
          ast,
          complexity,
          verifierComplexity,
          callableComplexities,
          userFunctionComplexities,
          globalVariableComplexities
        }
      };
    }
  } catch (e: any) {
    return typeof e === "object" ? { error: e.message } : { error: e };
  }
}

function wrappedRepl(opts?: { nodeUrl: string; chainId: string; address: string }): any {
  const repl =
    opts != null
      ? scalajs.repl(new scalajs.NodeConnectionSettings(opts.nodeUrl, opts.chainId.charCodeAt(0), opts.address))
      : scalajs.repl();

  const wrapReconfigure = (target: any): any => {
    const reconfigureFn = target.reconfigure.bind(target);
    return (newOpts: { nodeUrl: string; chainId: string; address: string }) => {
      const settings = new scalajs.NodeConnectionSettings(
        newOpts.nodeUrl,
        newOpts.chainId.charCodeAt(0),
        newOpts.address
      );
      const newRepl = reconfigureFn(settings);
      newRepl.reconfigure = wrapReconfigure(newRepl);
      return newRepl;
    };
  };

  repl.reconfigure = wrapReconfigure(repl);

  return repl;
}

const flattenCompilationResult = (compiled: any): any => {
  let result: any = {};
  if (compiled.error) {
    if (compiled.result) {
      const bytes = new Uint8Array(compiled.result);
      const base64 = crypto.base64Encode(bytes);
      result = { ...compiled, base64 };
      result.result && delete result.result;
    }
  } else {
    result = compiled.result;
  }
  return result;
};

const api: any = {
  compile: wrappedCompile,
  repl: wrappedRepl,
  get contractLimits() {
    return scalajs.contractLimits();
  },
  get version() {
    const version = scalajs.nodeVersion();
    return version && version.version;
  },
  scriptInfo: scalajs.scriptInfo,
  getTypes: scalajs.getTypes,
  getVarsDoc: scalajs.getVarsDoc,
  getFunctionsDoc: scalajs.getFunctionsDoc,
  decompile: scalajs.decompile,
  flattenCompilationResult,
  parseAndCompile: scalajs.parseAndCompile
};

(global as any).RideJS = api;
export = api;
