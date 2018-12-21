package com.wavesplatform.lang.compiler
import cats.kernel.Monoid
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, Version}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class ContractCompilerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("contract compiles when uses annotation bindings and correct return type") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(Version.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteVector) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet(List(DataEntry("a", a), DataEntry("sender", sender0)))
          | }
          |
          | @Verifier(t)
          | func verify() = {
          |   t.id == base58''
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val expectedResult = Right(
      Contract(
        List.empty,
        List(ContractFunction(
          CallableAnnotation("invocation"),
          None,
          Terms.FUNC(
            "foo",
            List("a"),
            BLOCKV2(
              LET("sender0", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
              FUNCTION_CALL(
                User("WriteSet"),
                List(FUNCTION_CALL(
                  Native(1102),
                  List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                       FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("sender0"))))
                ))
              )
            )
          )
        )),
        Some(VerifierFunction(
          VerifierAnnotation("t"),
          FUNC("verify", List.empty, FUNCTION_CALL(Native(FunctionIds.EQ), List(GETTER(REF("t"), "id"), CONST_BYTEVECTOR(ByteVector.empty))))
        ))
      ))
    compiler.ContractCompiler(ctx, expr) shouldBe expectedResult
  }

  property("contract compiles fails when incorrect return type") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteVector) = {
          |  a + invocation.caller.bytes
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("ContractFunction must return WriteSet/PaymentSet/ContractResult")
  }
}
