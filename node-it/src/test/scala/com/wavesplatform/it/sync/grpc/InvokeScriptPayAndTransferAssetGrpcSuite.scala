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
        miner.broadcastIssue(caller, "Asset", assetQuantity, 2, reissuable = true, fee = issueFee, waitForTx = true)
      )
      .explicitGet()
      .id()
      .toString

    val script = Right(Some(ScriptCompiler.compile("true", estimator).explicitGet()._1))
    smartAssetId = PBTransactions
      .vanilla(
        miner.broadcastIssue(caller, "Smart", assetQuantity, 2, reissuable = true, fee = issueFee, script = script, waitForTx = true)
      )
      .explicitGet()
      .id()
      .toString

    val scriptText  = "match tx {case _:TransferTransaction => false case _ => true}"
    val smartScript = Right(Some(ScriptCompiler.compile(scriptText, estimator).explicitGet()._1))
    rejAssetId = PBTransactions
      .vanilla(
        miner.broadcastIssue(caller, "Reject", assetQuantity, 2, reissuable = true, fee = issueFee, script = smartScript, waitForTx = true)
      )
      .explicitGet()
      .id()
      .toString
  }

  test("set script to dApp account and transfer out all waves") {
    val dAppBalance = miner.wavesBalance(dAppAddress)
    miner.broadcastTransfer(
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
         |let receiver = Address(base58'${receiver.toAddress.stringRepr}')
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
    miner.setScript(dApp, Right(Some(dAppScript)), fee = setScriptFee, waitForTx = true)
  }

  test("dApp can transfer payed asset if its own balance is 0") {
    val dAppInitBalance     = miner.wavesBalance(dAppAddress)
    val callerInitBalance   = miner.wavesBalance(callerAddress)
    val receiverInitBalance = miner.wavesBalance(receiverAddress)

    val paymentAmount = 10

    invoke("resendPayment", paymentAmount, assetId)

    miner.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular
    miner.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular - smartMinFee
    miner.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular

    miner.assetsBalance(dAppAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe paymentAmount - 1
    miner.assetsBalance(callerAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe assetQuantity - paymentAmount
    miner.assetsBalance(receiverAddress, Seq(assetId)).getOrElse(assetId, 0L) shouldBe 1
  }

  test("dApp can transfer payed smart asset if its own balance is 0") {
    val dAppInitBalance     = miner.wavesBalance(dAppAddress)
    val callerInitBalance   = miner.wavesBalance(callerAddress)
    val receiverInitBalance = miner.wavesBalance(receiverAddress)

    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    invoke("resendPayment", paymentAmount, smartAssetId, fee)

    miner.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular
    miner.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular - fee
    miner.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular

    miner.assetsBalance(dAppAddress, Seq(smartAssetId)).getOrElse(smartAssetId, 0L) shouldBe paymentAmount - 1
    miner.assetsBalance(callerAddress, Seq(smartAssetId)).getOrElse(smartAssetId, 0L) shouldBe assetQuantity - paymentAmount
    miner.assetsBalance(receiverAddress, Seq(smartAssetId)).getOrElse(smartAssetId, 0L) shouldBe 1
  }

  test("dApp can't transfer payed smart asset if it rejects transfers and its own balance is 0") {
    val dAppInitBalance     = miner.wavesBalance(dAppAddress)
    val callerInitBalance   = miner.wavesBalance(callerAddress)
    val receiverInitBalance = miner.wavesBalance(receiverAddress)

    val paymentAmount = 10
    val fee           = smartMinFee + smartFee * 2

    assertGrpcError(invoke("resendPayment", paymentAmount, rejAssetId, fee), "Transaction is not allowed by token-script")

    miner.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular
    miner.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular
    miner.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular

    miner.assetsBalance(dAppAddress, Seq(rejAssetId)).getOrElse(rejAssetId, 0L) shouldBe 0L
    miner.assetsBalance(callerAddress, Seq(rejAssetId)).getOrElse(rejAssetId, 0L) shouldBe assetQuantity
    miner.assetsBalance(receiverAddress, Seq(rejAssetId)).getOrElse(rejAssetId, 0L) shouldBe 0L
  }

  test("dApp can transfer payed Waves if its own balance is 0") {
    val dAppInitBalance     = miner.wavesBalance(dAppAddress)
    val callerInitBalance   = miner.wavesBalance(callerAddress)
    val receiverInitBalance = miner.wavesBalance(receiverAddress)

    dAppInitBalance.regular shouldBe 0

    val paymentAmount = 10
    invoke("resendPayment", paymentAmount)

    miner.wavesBalance(dAppAddress).regular shouldBe dAppInitBalance.regular + paymentAmount - 1
    miner.wavesBalance(callerAddress).regular shouldBe callerInitBalance.regular - paymentAmount - smartMinFee
    miner.wavesBalance(receiverAddress).regular shouldBe receiverInitBalance.regular + 1
  }

  def invoke(func: String, amount: Long, assetId: String = "WAVES", fee: Long = 500000): PBSignedTransaction = {
    val assetIdByteSting = if (assetId == "WAVES") ByteString.EMPTY else ByteString.copyFrom(Base58.decode(assetId))
    miner.broadcastInvokeScript(
      caller,
      Recipient().withPublicKeyHash(dAppAddress),
      Some(FUNCTION_CALL(FunctionHeader.User(func), List.empty)),
      payments = Seq(Amount.of(assetIdByteSting, amount)),
      fee = fee,
      waitForTx = true
    )
  }
}
