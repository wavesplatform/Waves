package com.wavesplatform.lang

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING, EVALUATED}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, ScriptResult}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient, Tx}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

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
                              args: List[Terms.EXPR] = List(Terms.CONST_BYTESTR(ByteStr.empty))): Either[ExecutionError, ScriptResult] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    ContractEvaluator(
      ctx.evaluationContext,
      compiled,
      Invocation(Terms.FUNCTION_CALL(FunctionHeader.User(func), args), Recipient.Address(callerAddress), callerPublicKey, None, ByteStr.empty)
    )
  }

  def parseCompileAndVerify(script: String, tx: Tx): Either[ExecutionError, EVALUATED] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()
    val evalm    = ContractEvaluator.verify(compiled.dec, compiled.vf.get, tx)
    EvaluatorV1.evalWithLogging(Right(ctx.evaluationContext), evalm)._2
  }

  property("Simple verify") {
    val dummyTx = Tx.Transfer(
      p = Tx.Proven(
        h = Tx.Header(id = ByteStr.empty, fee = 0, timestamp = 0, version = 0),
        sender = Recipient.Address(ByteStr.empty),
        bodyBytes = ByteStr.empty,
        senderPk = ByteStr.empty,
        proofs = IndexedSeq.empty
      ),
      feeAssetId = None,
      assetId = None,
      amount = 0,
      recipient = Recipient.Address(ByteStr.empty),
      attachment = ByteStr.empty
    )
    parseCompileAndVerify(
      """
        |let some = base58''
        |
        |func fooHelper2() = {
        |   false
        |}
        |
        |func fooHelper() = {
        |   fooHelper2() || false
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  t.senderPublicKey == some && fooHelper()
        |}
        |
      """.stripMargin,
      dummyTx
    ) shouldBe Testing.evaluated(false)
  }

  property("contract compiles if script uses InvokeScriptTransaction.fc field") {
    val bytes = ByteStr.fill(1)(1)
    val invokeScript = Tx.CI(
      p = Tx.Proven(
        h = Tx.Header(id = ByteStr.empty, fee = 0, timestamp = 0, version = 0),
        sender = Recipient.Address(ByteStr.empty),
        bodyBytes = ByteStr.empty,
        senderPk = ByteStr.empty,
        proofs = IndexedSeq.empty
      ),
      dappAddress = Recipient.Address(ByteStr.empty),
      maybePayment = None,
      feeAssetId = None,
      funcName = "foo",
      funcArgs = List(CONST_LONG(1), CONST_BOOLEAN(true), CONST_BYTESTR(bytes), CONST_STRING("ok"))
    )
    parseCompileAndVerify(
      s"""
         |
         | @Verifier(tx)
         | func verify() = {
         |   let expected = [1, true, base64'${bytes.base64}', "ok"]
         |   match tx {
         |     case ist: InvokeScriptTransaction =>
         |       ist.function == "foo"       &&
         |       ist.args[0]  == expected[0] &&
         |       ist.args[1]  == expected[1] &&
         |       ist.args[2]  == expected[2] &&
         |       ist.args[3]  == expected[3]
         |     case _ => false
         |   }
         | }
         |
        """.stripMargin,
      invokeScript
    ) shouldBe Testing.evaluated(true)
  }
}
