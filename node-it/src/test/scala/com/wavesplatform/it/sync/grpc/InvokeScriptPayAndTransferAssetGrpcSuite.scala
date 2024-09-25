package com.wavesplatform.it.sync.grpc

import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, PBTransactions, Recipient}
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokeScriptPayAndTransferAssetGrpcSuite extends GrpcBaseTransactionSuite {
  private val estimator = ScriptEstimatorV2

  private val (dApp, dAppAddress)         = (firstAcc, firstAddress)
  private val (caller, callerAddress)     = (secondAcc, secondAddress)
  private val (receiver, receiverAddress) = (thirdAcc, thirdAddress)

  val assetQuantity: Long  = 15
  var assetId: String      = ""
  var smartAssetId: String = ""
  var rejAssetId: String   = ""

  test("issue and transfer asset") {
    assetId = PBTransactions
      .vanilla(
        sender.broadcastIssue(caller, "Asset", assetQuantity, 2, reissuable = true, fee = issueFee, waitForTx = true),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString

    val script = Right(Some(ScriptCompiler.compile("true", estimator).explicitGet()._1))
    smartAssetId = PBTransactions
      .vanilla(
        sender.broadcastIssue(caller, "Smart", assetQuantity, 2, reissuable = true, fee = issueFee, script = script, waitForTx = true),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString

    val scriptText  = "match tx {case _:TransferTransaction => false case _ => true}"
    val smartScript = Right(Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1))
    rejAssetId = PBTransactions
      .vanilla(
        sender.broadcastIssue(caller, "Reject", assetQuantity, 2, reissuable = true, fee = issueFee, script = smartScript, waitForTx = true),
        unsafe = false
      )
      .explicitGet()
      .id()
      .toString
  }

  test("set script to dApp account and transfer out all waves") {
    val dAppBalance = sender.wavesBalance(dAppAddress)
    sender.broadcastTransfer(
      dApp,
      Recipient().withPublicKeyHash(callerAddress),
      dAppBalance.available - smartMinFee - setScriptFee,
      smartMinFee,
      waitForTx = true
    )

    val dAppScript = ScriptCompiler
      .compile(
        s"""
           |{-# STDLIB_VERSION 3 #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |let receiver = Address(base58'${receiver.toAddress.toString}')
           |
           |@Callable(i)
           |func resendPayment() = {
           |  if (isDefined(i.payment)) then
           |    let pay = extract(i.payment)
           |    TransferSet([ScriptTransfer(receiver, 1, pay.assetId)])
           |  else throw("need payment in WAVES or any Asset")
           |}
        """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1
    sender.setScript(dApp, Right(Some(dAppScript)), fee = setScriptFee, waitForTx = true)
  }

  test("dApp can transfer payed asset if its own balance is 0") {
    val dAppInitBalance     = sender.wavesBalance(dAppAddress)
    val callerInitBalance   = sender.wavesBalance(callerAddress)
    val receiverInitBalance = sender.wavesBalance(receiverAddress)

    val paymentAmount = 10

    invoke("resendPayment", paymentAmount, assetId)

    sender.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular
    sender.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular - smartMinFee
    sender.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular

    sender.assetsBalance(dAppAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe paymentAmount - 1
    sender.assetsBalance(callerAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe assetQuantity - paymentAmount
    sender.assetsBalance(receiverAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe 1
  }

  test("dApp can transfer payed smart asset if its own balance is 0") {
    val dAppInitBalance     = sender.wavesBalance(dAppAddress)
    val callerInitBalance   = sender.wavesBalance(callerAddress)
    val receiverInitBalance = sender.wavesBalance(receiverAddress)

    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    invoke("resendPayment", paymentAmount, smartAssetId, fee)

    sender.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular
    sender.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular - fee
    sender.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular

    sender.assetsBalance(dAppAddress, Seq(smartAssetId)).getOrElse(smartAssetId, 0L) shouldBe paymentAmount - 1
    sender.assetsBalance(callerAddress, Seq(smartAssetId)).getOrElse(smartAssetId, 0L) shouldBe assetQuantity - paymentAmount
    sender.assetsBalance(receiverAddress, Seq(smartAssetId)).getOrElse(smartAssetId, 0L) shouldBe 1
  }

  test("dApp can't transfer payed smart asset if it rejects transfers and its own balance is 0") {
    val dAppInitBalance     = sender.wavesBalance(dAppAddress)
    val callerInitBalance   = sender.wavesBalance(callerAddress)
    val receiverInitBalance = sender.wavesBalance(receiverAddress)

    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    assertGrpcError(invoke("resendPayment", paymentAmount, rejAssetId, fee), "Transaction is not allowed by token-script")

    sender.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular
    sender.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular
    sender.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular

    sender.assetsBalance(dAppAddress, Seq(rejAssetId)).getOrElse(rejAssetId, 0L) shouldBe 0L
    sender.assetsBalance(callerAddress, Seq(rejAssetId)).getOrElse(rejAssetId, 0L) shouldBe assetQuantity
    sender.assetsBalance(receiverAddress, Seq(rejAssetId)).getOrElse(rejAssetId, 0L) shouldBe 0L
  }

  test("dApp can transfer payed Waves if its own balance is 0") {
    val dAppInitBalance     = sender.wavesBalance(dAppAddress)
    val callerInitBalance   = sender.wavesBalance(callerAddress)
    val receiverInitBalance = sender.wavesBalance(receiverAddress)

    dAppInitBalance.regular shouldBe 0

    val paymentAmount = 10
    invoke("resendPayment", paymentAmount)

    sender.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular + paymentAmount - 1
    sender.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular - paymentAmount - smartMinFee
    sender.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular + 1
  }

  def invoke(func: String, amount: Long, assetId: String = "WAVES", fee: Long = 500000): PBSignedTransaction = {
    val assetIdByteSting = if (assetId == "WAVES") ByteString.EMPTY else ByteString.copyFrom(Base58.decode(assetId))
    sender.broadcastInvokeScript(
      caller,
      Recipient().withPublicKeyHash(dAppAddress),
      Some(FUNCTION_CALL(FunctionHeader.User(func), List.empty)),
      payments = Seq(Amount.of(assetIdByteSting, amount)),
      fee = fee,
      waitForTx = true
    )
  }
}
