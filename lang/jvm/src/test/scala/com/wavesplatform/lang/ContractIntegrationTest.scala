package com.wavesplatform.lang

import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult, EvaluatorV1, Log}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient, Tx}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {
  val ctx: CTX =
    PureContext.build(StdLibVersion.V3) |+|
      CTX(sampleTypes, Map.empty, Array.empty) |+|
      WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false)


  property("Simple test") {
    val src =
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
        |    then WriteSet(List(DataEntry("b", 1), DataEntry("sender", x)))
        |    else WriteSet(List(DataEntry("a", a), DataEntry("sender", x)))
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin

    val parsed = Parser.parseContract(src).get.value

    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    val expectedResult = ContractResult(
      List(
        DataItem.Bin("a", ByteStr.empty),
        DataItem.Bin("sender", ByteStr.empty)
      ),
      List()
    )

    val result = ContractEvaluator(
      ctx.evaluationContext,
      compiled,
      Invocation(Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List(Terms.CONST_BYTESTR(ByteStr.empty))), ByteStr.empty, None, ByteStr.empty)
    ).explicitGet()

  def parseCompileAndVerify(script: String, tx: Tx): (Log, Either[ExecutionError, EVALUATED]) = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()
    val evalm    = ContractEvaluator.verify(compiled.vf.get, tx)
    EvaluatorV1.evalWithLogging(ctx.evaluationContext, evalm)
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
    val (log, result) = parseCompileAndVerify(
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
    )
    result shouldBe Testing.evaluated(false)
  }

  property("Log big binaries") {
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
    val (log, result) = parseCompileAndVerify(
      s"""
        |let some = base64'${Base64.encode(new Array[Byte](2048))}'
        |
        |@Verifier(t)
        |func verify() = {
        |  t.senderPublicKey == some
        |}
        |
      """.stripMargin,
      dummyTx
    )
    log.toString.containsSlice("CONST_BYTESTR(base64:") should be(true)
    result shouldBe Testing.evaluated(false)
  }
}
