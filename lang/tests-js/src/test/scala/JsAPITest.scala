import com.wavesplatform.lang.directives.values.{V5, V6}
import utest.*

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
  }
}
