package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.transaction.DataTransactionData.DataEntry
import com.wavesplatform.protobuf.transaction.Recipient
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code

class InvokeScriptTransactionGrpcSuite extends GrpcBaseTransactionSuite {
  private val (dApp, dAppAddress) = (firstAcc, firstAddress)
  private val caller              = secondAcc

  test("set contract to dApp account") {
    val scriptText =
      """{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        | @Callable(inv)
        | func foo(a:ByteVector) = {
        |  WriteSet([DataEntry("a", a), DataEntry("sender", inv.caller.bytes)])
        | }
        |
        | @Callable(inv)
        | func default() = {
        |  WriteSet([DataEntry("a", "b"), DataEntry("sender", "senderId")])
        | }
        |
        |@Verifier(tx)
        |func verify() = {
        |    match tx {
        |        case TransferTransaction => false
        |        case _ => true
        |    }
        |}
        |""".stripMargin
    val script = ScriptCompiler.compile(scriptText, ScriptEstimatorV2).explicitGet()._1
    sender.setScript(dApp, Right(Some(script)), setScriptFee, waitForTx = true)

    val scriptInfo = sender.scriptInfo(firstAddress)

    scriptInfo.scriptBytes shouldBe ByteString.copyFrom(script.bytes())
  }

  test("dApp caller invokes a function on a dApp") {
    val arg = ByteStr(Array(42: Byte))

    sender.broadcastInvokeScript(
      caller,
      Recipient().withPublicKeyHash(dAppAddress),
      Some(FUNCTION_CALL(FunctionHeader.User("foo"), List(CONST_BYTESTR(arg).explicitGet()))),
      fee = 1.waves,
      waitForTx = true
    )

    sender.getDataByKey(dAppAddress, "a") shouldBe List(DataEntry("a", DataEntry.Value.BinaryValue(ByteString.copyFrom(arg))))
    sender.getDataByKey(dAppAddress, "sender") shouldBe List(
      DataEntry("sender", DataEntry.Value.BinaryValue(ByteString.copyFrom(caller.toAddress.bytes)))
    )
  }

  test("dApp caller invokes a default function on a dApp") {
    sender.broadcastInvokeScript(
      caller,
      Recipient().withPublicKeyHash(dAppAddress),
      functionCall = None,
      fee = 1.waves,
      waitForTx = true
    )
    sender.getDataByKey(dAppAddress, "a") shouldBe List(DataEntry("a", DataEntry.Value.StringValue("b")))
    sender.getDataByKey(dAppAddress, "sender") shouldBe List(DataEntry("sender", DataEntry.Value.StringValue("senderId")))
  }

  test("verifier works") {
    val dAppBalance = sender.wavesBalance(dAppAddress)
    assertGrpcError(
      sender.broadcastTransfer(dApp, Recipient().withPublicKeyHash(dAppAddress), transferAmount, minFee),
      "Transaction is not allowed by account-script",
      Code.INVALID_ARGUMENT
    )
    sender.wavesBalance(dAppAddress) shouldBe dAppBalance
  }
}
