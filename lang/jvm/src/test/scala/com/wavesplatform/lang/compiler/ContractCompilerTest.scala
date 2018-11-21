package com.wavesplatform.lang.compiler
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractCompilerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  private def treeTypeTest(
      propertyName: String)(expr: Expressions.CONTRACT, expectedResult: => Either[String, Contract], ctx: CompilerContext): Unit =
    property(propertyName) {
      compiler.ContractCompiler(ctx, expr) shouldBe expectedResult
    }

  treeTypeTest("contract compiles when uses annotation bindings")(
    ctx = compilerContext,
    expr = {
      val script =
        """
          |
          | @Callable(sender)
          | func foo(a:ByteVector) = {
          |  a + sender
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    },
    expectedResult = Right(
      Contract(
        List.empty,
        List(
          ContractFunction(CallableAnnotation("sender"),
                           None,
                           Terms.FUNC("foo", List("a"), FUNCTION_CALL(Native(203), List(REF("a"), REF("sender")))))),
        None
      )
    )
  )
}
