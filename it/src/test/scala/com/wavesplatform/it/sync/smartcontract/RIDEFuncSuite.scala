package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import org.scalatest.CancelAfterFailure

class RIDEFuncSuite extends BaseTransactionSuite with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private val acc0 = pkByAddress(firstAddress)

  test("assetBalance() verification") {
    val asset = sender
      .issue(acc0.address, "SomeCoin", "SomeDescription", someAssetAmount, 0, reissuable = false, issueFee, 2, waitForTx = true)
      .id

    val newAddress   = sender.createAddress()
    val pkNewAddress = pkByAddress(newAddress)

    sender.transfer(acc0.address, newAddress, 10.waves, minFee, waitForTx = true)

    val scriptSrc =
      s"""
         |match tx {
         |  case tx : SetScriptTransaction => true
         |  case other => assetBalance(tx.sender, base58'$asset') > 0
         |}
      """.stripMargin

    val compiled = ScriptCompiler(scriptSrc, isAssetScript = false).explicitGet()._1

    val tx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(pkNewAddress, Some(compiled), setScriptFee, System.currentTimeMillis()).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
          .explicitGet()
          .json()),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(IssuedAsset(ByteStr.decodeBase58(asset).get),
                    acc0,
                    pkNewAddress,
                    100000000,
                    System.currentTimeMillis(),
                    Waves,
                    smartMinFee,
                    Array())
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transfer = sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
        .explicitGet()
        .json())
    nodes.waitForHeightAriseAndTxPresent(transfer.id)

    val udpatedScript =
      s"""
         |match tx {
         |  case tx : SetScriptTransaction => true
         |  case other => assetBalance(tx.sender, base58'$asset') >= 900000000 && wavesBalance(tx.sender) >500000000
         |}
      """.stripMargin

    val updated = ScriptCompiler(udpatedScript, isAssetScript = false).explicitGet()._1

    val updTx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(pkNewAddress, Some(updated), setScriptFee + smartFee, System.currentTimeMillis()).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(updTx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
          .explicitGet()
          .json()),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(IssuedAsset(ByteStr.decodeBase58(asset).get),
                    acc0,
                    pkNewAddress,
                    800000000,
                    System.currentTimeMillis(),
                    Waves,
                    smartMinFee,
                    Array())
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transferAfterUpd = sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(Waves, pkNewAddress, pkNewAddress, 1.waves, System.currentTimeMillis(), Waves, smartMinFee, Array())
        .explicitGet()
        .json())
    nodes.waitForHeightAriseAndTxPresent(transferAfterUpd.id)
  }
}
