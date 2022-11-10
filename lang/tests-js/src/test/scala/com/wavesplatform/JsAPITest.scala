package com.wavesplatform

import com.wavesplatform.lang.directives.values.{V5, V6}
import utest.*

import scala.scalajs.js.{Array, Dynamic, JSON}

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
  }
}
