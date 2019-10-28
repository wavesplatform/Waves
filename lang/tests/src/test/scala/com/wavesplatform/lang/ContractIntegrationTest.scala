package com.wavesplatform.lang

import cats.Id
import cats.syntax.monoid._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, sampleTypes}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.Invocation
import com.wavesplatform.lang.v1.evaluator._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient, Tx}
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractIntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {

  private val ctx: CTX[Environment] =
      PureContext.build(Global, V3).withEnvironment[Environment] |+|
      CTX[Environment](sampleTypes, Map.empty, Array.empty) |+|
      WavesContext.build(
        DirectiveSet(V3, Account, DApp).explicitGet()
      )

  private val environment: Environment[Id] =
    Common.emptyBlockchainEnvironment()

  private val callerAddress: ByteStr      = ByteStr.fromLong(1)
  private val callerPublicKey: ByteStr    = ByteStr.fromLong(2)
  private val transactionId: ByteStr      = ByteStr.fromLong(777)
  private val fee: Int                    = 1000 * 1000
  private val feeAssetId: Option[ByteStr] = None

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
        |    else WriteSet(
        |      [
        |         DataEntry("caller", x),
        |         DataEntry("callerPk", y),
        |         DataEntry("transactionId", invocation.transactionId),
        |         DataEntry("fee",           invocation.fee),
        |         DataEntry("feeAssetId",    match invocation.feeAssetId  {
        |                                      case custom: ByteVector => custom
        |                                      case waves:  Unit       => base64''
        |                                    }
        |         )
        |      ]
        |    )
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin,
      "foo"
    ).explicitGet()._1 shouldBe ScriptResult(
      List(
        DataItem.Bin("caller", callerAddress),
        DataItem.Bin("callerPk", callerPublicKey),
        DataItem.Bin("transactionId", transactionId),
        DataItem.Lng("fee", fee),
        DataItem.Bin("feeAssetId", ByteStr.empty),
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
    ).explicitGet()._1 shouldBe ScriptResult(List(DataItem.Lng("1", 22)), List())
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
      "foo",
      args = Nil
    )
    inside(evalResult) {
      case Left((error, log)) =>
        error shouldBe "exception message"
        log should contain allOf (
          ("a", Right(CONST_LONG(1))),
          ("b", Right(CONST_LONG(2))),
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
                              func  : String,
                              args  : List[Terms.EXPR] = List(Terms.CONST_BYTESTR(ByteStr.empty).explicitGet())
                             ): Either[(ExecutionError, Log[Id]), (ScriptResult, Log[Id])] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()

    ContractEvaluator(
      ctx.evaluationContext(environment),
      compiled,
      Invocation(
        Terms.FUNCTION_CALL(FunctionHeader.User(func), args),
        Recipient.Address(callerAddress),
        callerPublicKey,
        None,
        ByteStr.empty,
        transactionId,
        fee,
        feeAssetId
      )
    )
  }

  def parseCompileAndVerify(script: String, tx: Tx): Either[ExecutionError, EVALUATED] = {
    val parsed   = Parser.parseContract(script).get.value
    val compiled = ContractCompiler(ctx.compilerContext, parsed).explicitGet()
    val evalm    = ContractEvaluator.verify(compiled.decs, compiled.verifierFuncOpt.get, tx)
    EvaluatorV1().evalWithLogging(Right(ctx.evaluationContext(environment)), evalm)._2
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
      dAppAddressOrAlias = Recipient.Address(ByteStr.empty),
      maybePayment = None,
      feeAssetId = None,
      funcName = Some("foo"),
      funcArgs = List(CONST_LONG(1), CONST_BOOLEAN(true), CONST_BYTESTR(bytes).explicitGet(), CONST_STRING("ok").explicitGet())
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

  property("User function should return correct lists for sets") {
    parseCompileAndEvaluate(
      """
        | {-#STDLIB_VERSION 3#-}
        | {-#SCRIPT_TYPE ACCOUNT#-}
        | {-#CONTENT_TYPE DAPP#-}
        |
        | func wSet() = {
        |     [
        |       DataEntry("a", 1),
        |       DataEntry("b", true),
        |       DataEntry("c", "str"),
        |       DataEntry("d",  toBytes(256))
        |     ]
        | }
        |
        | func tSet(caller: Address) = {
        |     [
        |       ScriptTransfer(caller, 1, unit),
        |       ScriptTransfer(caller, 2, unit)
        |     ]
        | }
        |
        | @Callable(i)
        | func test() = {
        |     ScriptResult(WriteSet(wSet()), TransferSet(tSet(i.caller)))
        | }
        |
        """.stripMargin,
      "test",
      args = Nil
    ).explicitGet()._1 shouldBe ScriptResult(
      List(
        DataItem.Lng("a", 1),
        DataItem.Bool("b", true),
        DataItem.Str("c", "str"),
        DataItem.Bin("d", ByteStr.fromLong(256L))
      ),
      List(
        (Recipient.Address(callerAddress), 1L, None),
        (Recipient.Address(callerAddress), 2L, None)
      )
    )
  }

  property("script result fields") {
    parseCompileAndEvaluate(
      """
        | {-# STDLIB_VERSION 3       #-}
        | {-# SCRIPT_TYPE    ACCOUNT #-}
        | {-# CONTENT_TYPE   DAPP    #-}
        |
        | func scriptResult1(caller: Address) =
        |     ScriptResult(
        |       WriteSet(
        |         [
        |           DataEntry("a", 1),
        |           DataEntry("b", 2)
        |         ]
        |       ),
        |       TransferSet(
        |         [
        |           ScriptTransfer(caller, 1, unit),
        |           ScriptTransfer(caller, 2, unit)
        |         ]
        |       )
        |     )
        |
        | func scriptResult2(caller: Address) =
        |     ScriptResult(
        |       WriteSet(
        |         [
        |           DataEntry("c", 3),
        |           DataEntry("d", 4)
        |         ]
        |       ),
        |       TransferSet(
        |         [
        |           ScriptTransfer(caller, 3, unit),
        |           ScriptTransfer(caller, 4, unit)
        |         ]
        |       )
        |     )
        |
        | @Callable(i)
        | func test() = {
        |   let sr1 = scriptResult1(i.caller)
        |   let sr2 = scriptResult2(i.caller)
        |
        |   let writes    = if (true)  then sr1.writeSet.data         else sr2.writeSet.data
        |   let transfers = if (false) then sr1.transferSet.transfers else sr2.transferSet.transfers
        |
        |   ScriptResult(
        |     WriteSet(writes),
        |     TransferSet(transfers)
        |   )
        | }
        |
        """.stripMargin,
      "test",
      args = Nil
    ).explicitGet()._1 shouldBe ScriptResult(
      List(
        DataItem.Lng("a", 1),
        DataItem.Lng("b", 2)
      ),
      List(
        (Recipient.Address(callerAddress), 3, None),
        (Recipient.Address(callerAddress), 4, None)
      )
    )
  }
}
