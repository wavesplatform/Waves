package com.wavesplatform

import com.wavesplatform.lang.directives.values.{V5, V6}
import utest.*

import scala.scalajs.js.{Array, Dictionary, Dynamic, JSON}

object JsAPITest extends JsTestBase {
  private def simpleDApp(result: String): String =
    dApp(
      s"""
         |@Callable(i)
         |func f() = $result
       """.stripMargin,
      V6
    )

  val tests: Tests = Tests {
    test("expression error and success") {
      assertCompileError("1 + 1", "Script should return boolean")
      assertCompileSuccess("true")
    }

    test("dApp error and success") {
      assertCompileError(simpleDApp("true"), "CallableFunction needs to return")
      assertCompileSuccess(simpleDApp("[]"))
    }

    test("expression complexity") {
      expressionComplexity("sigVerify(base16'', base58'', base64'')", V5) ==> 200
      expressionComplexity("sigVerify(base16'', base58'', base64'')") ==> 180
    }

    test("dApp complexities") {
      val r = dAppComplexities(
        """
          | let x = 1 + 1
          | func f(list: List[Int]) =list.size()
          |
          | @Callable(i)
          | func c1() = []
          |
          | @Callable(i)
          | func c2() = [IntegerEntry("key", x)]
          |
          | @Verifier(tx)
          | func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
        """.stripMargin
      )
      r.complexity ==> 182
      r.verifierComplexity ==> 182
      r.callableComplexities ==> Map("c1" -> 1, "c2" -> 4)
      r.userFunctionComplexities ==> Map("f" -> 2)
      r.globalVariableComplexities ==> Map("x" -> 1)
    }

    test("AST result type for declarations") {
      val compiled = JsAPI.parseAndCompile(
        dApp(
          """
            | func sum(acc: List[Int], elem: Int) = acc :+ elem
            | let arr     = [1, 2, 3, 4, 5]
            | let letFold = FOLD<5>(arr, [], sum)
            |
            | @Callable(i)
            | func default() = {
            |   let letCall  = i.caller.toString()
            |   let letIf    = if (true) then 1 else ""
            |   let letMatch = match letIf {
            |     case _: Int   => true
            |     case _: String => Address(base58'')
            |   }
            |   func funcRef() = letCall
            |   []
            | }
          """.stripMargin,
          V6
        ),
        3
      )
      val callables = compiled.dAppAst.annFuncList.asInstanceOf[Array[Dynamic]]

      val invocation = callables(0).func.expr.dec.expr.args.asInstanceOf[Array[Dynamic]].apply(0).ref
      invocation.name ==> "i"
      invocation.resultType.`type` ==> "Invocation"

      val letCall = callables(0).func.expr.dec
      letCall.name.value ==> "letCall"
      letCall.expr.resultType.`type` ==> "String"

      val letIf = callables(0).func.expr.body.dec
      letIf.name.value ==> "letIf"
      JSON.stringify(letIf.expr.resultType.unionTypes) ==> """[{"type":"Int"},{"type":"String"}]"""

      val letMatch = callables(0).func.expr.body.body.dec
      letMatch.name.value ==> "letMatch"
      JSON.stringify(letMatch.expr.resultType.unionTypes) ==> """[{"type":"Boolean"},{"type":"Address"}]"""

      val funcRef = callables(0).func.expr.body.body.body.dec
      funcRef.name.value ==> "funcRef"
      funcRef.expr.resultType.`type` ==> "String"

      val letFold = compiled.dAppAst.decList.asInstanceOf[Array[Dynamic]].apply(2)
      letFold.name.value ==> "letFold"
      JSON.stringify(letFold.expr.resultType) ==> """{"listOf":{"type":"Int"}}"""
    }

    test("AST result should be fixed while using libraries") {
      val script =
        """
          | {-# SCRIPT_TYPE ACCOUNT #-}
          | {-# IMPORT lib1, lib2, lib3 #-}
          | let a = 5
          | func f() = 3
          | true
        """.stripMargin

      val import1 =
        "lib1" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = a + 1
          """.stripMargin

      val anotherImport1 =
        "lib1" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func inc(a: Int) = {
            |   if (true) then throw() else a + 1
            | }
          """.stripMargin

      val import2 =
        "lib2" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func dec(a: Int) = a - 1
          """.stripMargin

      val import3 =
        "lib3" ->
          """
            | {-# SCRIPT_TYPE  ACCOUNT #-}
            | {-# CONTENT_TYPE LIBRARY #-}
            | func multiply(a: Int, b: Int) = a * b
          """.stripMargin

      val r1 = JsAPI.parseAndCompile(script, 3, libraries = Dictionary(import1, import2, import3))
      val r2 = JsAPI.parseAndCompile(script, 3, libraries = Dictionary(anotherImport1, import2, import3))

      def checkPos(expr: Dynamic) = {
        val let = expr.exprAst.expr.body.body.body.dec
        let.`type` ==> "LET"
        let.name.value ==> "a"
        let.posStart ==> 64
        let.posEnd ==> 73

        val func = expr.exprAst.expr.body.body.body.body.dec
        func.`type` ==> "FUNC"
        func.name.value ==> "f"
        func.posStart ==> 75
        func.posEnd ==> 87
      }

      checkPos(r1)
      checkPos(r2)
    }

    test("correct AST for library") {
      val library =
        """
          | {-# SCRIPT_TYPE  ACCOUNT #-}
          | {-# CONTENT_TYPE LIBRARY #-}
          | func f() = 1
        """.stripMargin

      val result = JsAPI.parseAndCompile(library, 3)
      val expected = """
                       |{
                       |  "type": "BLOCK",
                       |  "posStart": 62,
                       |  "posEnd": 88,
                       |  "resultType": {
                       |    "type": "Boolean"
                       |  },
                       |  "ctx": [],
                       |  "dec": {
                       |    "type": "FUNC",
                       |    "posStart": 62,
                       |    "posEnd": 74,
                       |    "name": {
                       |      "value": "f",
                       |      "posStart": 67,
                       |      "posEnd": 68
                       |    },
                       |    "argList": [],
                       |    "expr": {
                       |      "type": "CONST_LONG",
                       |      "posStart": 73,
                       |      "posEnd": 74,
                       |      "resultType": {
                       |        "type": "Int"
                       |      },
                       |      "ctx": []
                       |    }
                       |  },
                       |  "body": {
                       |    "type": "TRUE",
                       |    "posStart": 84,
                       |    "posEnd": 88,
                       |    "resultType": {
                       |      "type": "Boolean"
                       |    },
                       |    "ctx": []
                       |  }
                       |}
                     """.stripMargin
      JSON.stringify(result.exprAst.expr) ==> JSON.stringify(JSON.parse(expected))
      JSON.stringify(result.errorList) ==> "[]"
    }
  }
}
