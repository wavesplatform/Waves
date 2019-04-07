package com.wavesplatform.lang

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, TRUE}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, Log, ScriptResult}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {

  val ctx: CTX =
    PureContext.build(V3) |+|
      CTX(sampleTypes, Map.empty, Array.empty) |+|
      WavesContext.build(
        DirectiveSet(V3, Account, DApp).explicitGet(),
        Common.emptyBlockchainEnvironment()
      )

  private val callerAddress: ByteStr   = ByteStr.fromLong(1)
  private val callerPublicKey: ByteStr = ByteStr.fromLong(2)
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
        |  let y = invocation.callerPublicKey
        |  if (fooHelper())
        |    then WriteSet([DataEntry("b", 1), DataEntry("sender", x)])
        |    else WriteSet([DataEntry("caller", x), DataEntry("callerPk", y)])
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin,
      "foo"
    ).explicitGet() shouldBe ScriptResult(
      List(
        DataItem.Bin("caller", callerAddress),
        DataItem.Bin("callerPk", callerPublicKey)
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
    ).explicitGet() shouldBe ScriptResult(List(DataItem.Lng("1", 22)), List())
  }

  property("@Callable exception error contains initialised values") {
    val evalResult = parseCompileAndEvaluate(
      """
        | @Callable(invocation)
        | func foo() = {
        |   let a = 1
        |   let b = 2
        |   let isError = a != b
        |   if (isError)
        |     then throw("exception message")
        |     else WriteSet([])
        | }
      """.stripMargin,
      "foo"
    )
    inside(evalResult) {
      case Left((error, log)) =>
        error shouldBe "exception message"
        log should contain allOf(
          ("a",       Right(CONST_LONG(1))),
          ("b",       Right(CONST_LONG(2))),
          ("isError", Right(TRUE))
        )
    }
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
                              args: List[Terms.EXPR] = List(Terms.CONST_BYTESTR(ByteStr.empty))): Either[(ExecutionError, Log), ScriptResult] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    ContractEvaluator(
      ctx.evaluationContext,
      compiled,
      Invocation(Terms.FUNCTION_CALL(FunctionHeader.User(func), args), Recipient.Address(callerAddress), callerPublicKey, None, ByteStr.empty)
    )
  }

}
