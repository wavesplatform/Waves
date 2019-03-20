package com.wavesplatform.lang

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.domain.DataItem
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

  val ctx: CTX =
    PureContext.build(StdLibVersion.V3) |+|
      CTX(sampleTypes, Map.empty, Array.empty) |+|
      WavesContext.build(DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Contract), Common.emptyBlockchainEnvironment())

  property("Simple call") {
    parseCompileAndEvaluate(
      """
        |
        |func fooHelper2() = {
        |   false
        |}
        |
        |func fooHelper() = {
        |   fooHelper2() || false
        |}
        |
        |@Callable(invocation)
        |func foo(a:ByteVector) = {
        |  let x = invocation.caller.bytes
        |  if (fooHelper())
        |    then WriteSet([DataEntry("b", 1), DataEntry("sender", x)])
        |    else WriteSet([DataEntry("a", a), DataEntry("sender", x)])
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin,
      "foo"
    ).explicitGet() shouldBe ContractResult(
      List(
        DataItem.Bin("a", ByteStr.empty),
        DataItem.Bin("sender", ByteStr.empty)
      ),
      List()
    )
  }

  property("Callable can have 22 args") {
    parseCompileAndEvaluate(
      """
        |@Callable(invocation)
        |func foo(a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, a9:Int, a10:Int,
        |         a11:Int, a12:Int, a13:Int, a14:Int, a15:Int, a16:Int, a17:Int, a18:Int, a19:Int, a20:Int,
        |         a21:Int, a22:Int) = { WriteSet([DataEntry(toString(a1), a22)]) }
      """.stripMargin,
      "foo",
      Range(1, 23).map(i => Terms.CONST_LONG(i)).toList
    ).explicitGet() shouldBe ContractResult(List(DataItem.Lng("1", 22)), List())
  }

  property("Callable can't have more than 22 args") {
    val src =
      """
        |@Callable(invocation)
        |func foo(a1:Int, a2:Int, a3:Int, a4:Int, a5:Int, a6:Int, a7:Int, a8:Int, a9:Int, a10:Int,
        |         a11:Int, a12:Int, a13:Int, a14:Int, a15:Int, a16:Int, a17:Int, a18:Int, a19:Int, a20:Int,
        |         a21:Int, a22:Int, a23:Int) = { throw() }
      """.stripMargin

    val parsed = Parser.parseContract(src).get.value

    ContractCompiler(ctx.compilerContext, parsed) should produce("no more than 22 arguments")
  }

  def parseCompileAndEvaluate(script: String,
                              func: String,
                              args: List[Terms.EXPR] = List(Terms.CONST_BYTESTR(ByteStr.empty))): Either[ExecutionError, ContractResult] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    ContractEvaluator(
      ctx.evaluationContext,
      compiled,
      Invocation(Terms.FUNCTION_CALL(FunctionHeader.User(func), args), ByteStr.empty, None, ByteStr.empty)
    )
  }

}
