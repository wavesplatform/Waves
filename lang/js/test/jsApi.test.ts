import { describe, test, expect } from "vitest";
import { parseAndCompile } from "./helpers/compiler";
import {
  assertCompileError,
  assertCompileSuccess,
  expressionComplexity,
  dAppComplexities,
  dApp,
  V5,
  V6
} from "./helpers/jsTestBase";

// Mirrors Scala's String.stripMargin: strips leading whitespace + '|' per line.
// Needed for the position-sensitive AST tests, whose expected posStart/posEnd
// offsets depend on the exact byte layout of the original Scala literals.
function stripMargin(s: string): string {
  return s
    .split("\n")
    .map((line) => line.replace(/^\s*\|/, ""))
    .join("\n");
}

function simpleDApp(result: string): string {
  return dApp(
    `
@Callable(i)
func f() = ${result}
       `,
    V6
  );
}

describe("JsAPITest", () => {
  test("expression error and success", () => {
    assertCompileError("1 + 1", "Script should return boolean");
    assertCompileSuccess("true");
  });

  test("dApp error and success", () => {
    assertCompileError(simpleDApp("true"), "CallableFunction needs to return");
    assertCompileSuccess(simpleDApp("[]"));
  });

  test("expression complexity", () => {
    expect(expressionComplexity("sigVerify(base16'', base58'', base64'')", V5)).toBe(200);
    expect(expressionComplexity("sigVerify(base16'', base58'', base64'')")).toBe(180);
  });

  test("dApp complexities", () => {
    const r = dAppComplexities(
      `
 let x = 1 + 1
 func f(list: List[Int]) =list.size()

 @Callable(i)
 func c1() = []

 @Callable(i)
 func c2() = [IntegerEntry("key", x)]

 @Verifier(tx)
 func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
        `
    );
    expect(r.complexity).toBe(182);
    expect(r.verifierComplexity).toBe(182);
    expect(r.callableComplexities).toEqual({ c1: 1, c2: 4 });
    expect(r.userFunctionComplexities).toEqual({ f: 2 });
    expect(r.globalVariableComplexities).toEqual({ x: 1 });
  });

  test("AST result type for declarations", () => {
    const compiled = parseAndCompile(
      dApp(
        `
 func sum(acc: List[Int], elem: Int) = acc :+ elem
 let arr     = [1, 2, 3, 4, 5]
 let letFold = FOLD<5>(arr, [], sum)

 @Callable(i)
 func default() = {
   let letCall  = i.caller.toString()
   let letIf    = if (true) then 1 else ""
   let letMatch = match letIf {
     case _: Int   => true
     case _: String => Address(base58'')
   }
   func funcRef() = letCall
   []
 }
          `,
        V6
      ),
      3
    );
    const callables = compiled.dAppAst.annFuncList;

    const invocation = callables[0].func.expr.dec.expr.args[0].ref;
    expect(invocation.name).toBe("i");
    expect(invocation.resultType.type).toBe("Invocation");

    const letCall = callables[0].func.expr.dec;
    expect(letCall.name.value).toBe("letCall");
    expect(letCall.expr.resultType.type).toBe("String");

    const letIf = callables[0].func.expr.body.dec;
    expect(letIf.name.value).toBe("letIf");
    expect(JSON.stringify(letIf.expr.resultType.unionTypes)).toBe('[{"type":"Int"},{"type":"String"}]');

    const letMatch = callables[0].func.expr.body.body.dec;
    expect(letMatch.name.value).toBe("letMatch");
    expect(JSON.stringify(letMatch.expr.resultType.unionTypes)).toBe('[{"type":"Boolean"},{"type":"Address"}]');

    const funcRef = callables[0].func.expr.body.body.body.dec;
    expect(funcRef.name.value).toBe("funcRef");
    expect(funcRef.expr.resultType.type).toBe("String");

    const letFold = compiled.dAppAst.decList[2];
    expect(letFold.name.value).toBe("letFold");
    expect(JSON.stringify(letFold.expr.resultType)).toBe('{"listOf":{"type":"Int"}}');
  });

  test("AST result should be fixed while using libraries", () => {
    const script = stripMargin(`
          | {-# SCRIPT_TYPE ACCOUNT #-}
          | {-# IMPORT lib1, lib2, lib3 #-}
          | let a = 5
          | func f() = 3
          | true
        `);

    const import1 = stripMargin(`
          | {-# SCRIPT_TYPE  ACCOUNT #-}
          | {-# CONTENT_TYPE LIBRARY #-}
          | func inc(a: Int) = a + 1
          `);

    const anotherImport1 = stripMargin(`
          | {-# SCRIPT_TYPE  ACCOUNT #-}
          | {-# CONTENT_TYPE LIBRARY #-}
          | func inc(a: Int) = {
          |   if (true) then throw() else a + 1
          | }
          `);

    const import2 = stripMargin(`
          | {-# SCRIPT_TYPE  ACCOUNT #-}
          | {-# CONTENT_TYPE LIBRARY #-}
          | func dec(a: Int) = a - 1
          `);

    const import3 = stripMargin(`
          | {-# SCRIPT_TYPE  ACCOUNT #-}
          | {-# CONTENT_TYPE LIBRARY #-}
          | func multiply(a: Int, b: Int) = a * b
          `);

    const r1 = parseAndCompile(script, 3, false, false, { lib1: import1, lib2: import2, lib3: import3 });
    const r2 = parseAndCompile(script, 3, false, false, { lib1: anotherImport1, lib2: import2, lib3: import3 });

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    function checkPos(expr: any): void {
      const letDec = expr.exprAst.expr.body.body.body.dec;
      expect(letDec.type).toBe("LET");
      expect(letDec.name.value).toBe("a");
      expect(letDec.posStart).toBe(64);
      expect(letDec.posEnd).toBe(73);

      const func = expr.exprAst.expr.body.body.body.body.dec;
      expect(func.type).toBe("FUNC");
      expect(func.name.value).toBe("f");
      expect(func.posStart).toBe(75);
      expect(func.posEnd).toBe(87);
    }

    checkPos(r1);
    checkPos(r2);
  });

  test("correct AST for library", () => {
    const library = stripMargin(`
          | {-# SCRIPT_TYPE  ACCOUNT #-}
          | {-# CONTENT_TYPE LIBRARY #-}
          | func f() = 1
        `);

    const result = parseAndCompile(library, 3);
    const expected = `
{
  "dec": {
    "type": "FUNC",
    "posStart": 62,
    "posEnd": 74,
    "name": {
      "value": "f",
      "posStart": 67,
      "posEnd": 68
    },
    "argList": [],
    "expr": {
      "type": "CONST_LONG",
      "posStart": 73,
      "posEnd": 74,
      "resultType": {
        "type": "Int"
      },
      "ctx": []
    }
  },
  "body": {
    "type": "TRUE",
    "posStart": 84,
    "posEnd": 88,
    "resultType": {
      "type": "Boolean"
    },
    "ctx": []
  },
  "type": "BLOCK",
  "posStart": 62,
  "posEnd": 88,
  "resultType": {
    "type": "Boolean"
  },
  "ctx": []
}
                     `;
    expect(JSON.stringify(result.exprAst.expr)).toBe(JSON.stringify(JSON.parse(expected)));
    expect(JSON.stringify(result.errorList)).toBe("[]");
  });

  test("ill-formed characters", () => {
    const invalidChar = "\ud87e";
    const script = `
{-# STDLIB_VERSION 6 #-}
{-# CONTENT_TYPE DAPP #-}
{-# SCRIPT_TYPE ACCOUNT #-}

func call(a: String, b: Int) = {
    let zzz = "aaa${invalidChar}bbb"
    ([], zzz)
}
        `;
    assertCompileError(script, "contains ill-formed characters");
  });
});
