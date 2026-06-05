// Public @waves/ride-js API. Ported from the original ride-js src/index.js.
// The single substantive change vs. the original: instead of requiring the two
// separate npm packages (@waves/ride-lang + @waves/ride-repl) — each carrying its
// own copy of the ScalaJS runtime — this requires ONE combined ScalaJS artifact
// (scalajs/ride-scalajs.js, the linked repl-js output) that exports both the
// compiler API and the repl API. So ScalaJS is bundled once.
/* eslint-disable @typescript-eslint/no-explicit-any, @typescript-eslint/no-var-requires */
require("./interop"); // side effect: installs crypto/http globals before the artifact runs
const crypto: any = require("@waves/ts-lib-crypto");
const scalajs: any = require("../scalajs/ride-scalajs.js");

// The combined artifact exposes both the compiler exports (from lang-js JsAPI)
// and the repl exports (from repl-js JsAPI) as one module.
const scalaJsCompiler = scalajs;
const replJs = scalajs;

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
    const result = scalaJsCompiler.compile(code, estimatorVersion, needCompaction, removeUnusedCode, libraries);
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
    console.log(e);
    return typeof e === "object" ? { error: e.message } : { error: e };
  }
}

function wrappedRepl(opts?: { nodeUrl: string; chainId: string; address: string }): any {
  const repl =
    opts != null
      ? replJs.repl(new replJs.NodeConnectionSettings(opts.nodeUrl, opts.chainId.charCodeAt(0), opts.address))
      : replJs.repl();

  const wrapReconfigure = (repl: any): any => {
    const reconfigureFn = repl.reconfigure.bind(repl);
    return (opts: { nodeUrl: string; chainId: string; address: string }) => {
      const settings = new replJs.NodeConnectionSettings(opts.nodeUrl, opts.chainId.charCodeAt(0), opts.address);
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
    return scalaJsCompiler.contractLimits();
  },
  get version() {
    const version = scalaJsCompiler.nodeVersion();
    return version && version.version;
  },
  scriptInfo: scalaJsCompiler.scriptInfo,
  getTypes: scalaJsCompiler.getTypes,
  getVarsDoc: scalaJsCompiler.getVarsDoc,
  getFunctionsDoc: scalaJsCompiler.getFunctionsDoc,
  decompile: scalaJsCompiler.decompile,
  flattenCompilationResult,
  parseAndCompile: scalaJsCompiler.parseAndCompile
};

(global as any).RideJS = api;
export = api;
