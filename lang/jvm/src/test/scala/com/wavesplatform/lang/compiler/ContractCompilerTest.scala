package com.wavesplatform.lang.compiler
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, Version}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractCompilerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  private def treeTypeTest(
      propertyName: String)(expr: Expressions.CONTRACT, expectedResult: => Either[String, Contract], ctx: CompilerContext): Unit =
    property(propertyName) {
      compiler.ContractCompiler(ctx, expr) shouldBe expectedResult
    }

  treeTypeTest("contract compiles when uses annotation bindings and correct return type")(
    ctx = Monoid.combine(compilerContext, WavesContext.build(Version.V3, Common.emptyBlockchainEnvironment(), false).compilerContext),
    expr = {
      val script =
        """
          |
          | @Callable(sender)
          | func foo(a:ByteVector) = {
          |  WriteSet(List(DataEntry("a", a), DataEntry("sender", sender)))
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    },
    expectedResult = Right(
      Contract(
        List.empty,
        List(ContractFunction(
          CallableAnnotation("sender"),
          None,
          Terms.FUNC(
            "foo",
            List("a"),
            FUNCTION_CALL(
              User("WriteSet"),
              List(FUNCTION_CALL(
                Native(1102),
                List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                     FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("sender"))))
              ))
            )
          )
        )),
        None
      ))
  )

  treeTypeTest("contract compiles fails when incorrect return type")(
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
    expectedResult = Left("Compilation failed: ContractFunction must return WriteSet, but got 'ByteVector' in 0-0")
  )

}
