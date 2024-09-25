package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalatest.CancelAfterFailure

class RideFuncSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val estimator = ScriptEstimatorV2

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  test("assetBalance() verification") {
    val asset = sender
      .issue(firstKeyPair, "SomeCoin", "SomeDescription", someAssetAmount, 0, reissuable = false, issueFee, 2, waitForTx = true)
      .id

    val newAddress   = sender.createKeyPair()
    val pkNewAddress = newAddress

    sender.transfer(firstKeyPair, newAddress.toAddress.toString, 10.waves, minFee, waitForTx = true)

    val scriptSrc =
      s"""
         |match tx {
         |  case _: SetScriptTransaction => true
         |  case _ => assetBalance(tx.sender, base58'$asset') > 0
         |}
      """.stripMargin

    val compiled = ScriptCompiler.compile(scriptSrc, estimator).explicitGet()._1

    val tx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(1.toByte, pkNewAddress, Some(compiled), setScriptFee, System.currentTimeMillis()).explicitGet().json()
      )
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransaction
          .selfSigned(2.toByte, pkNewAddress, pkNewAddress.toAddress, Waves, 1.waves, Waves, smartMinFee, ByteStr.empty, System.currentTimeMillis())
          .explicitGet()
          .json()
      ),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransaction
        .selfSigned(
          2.toByte,
          firstKeyPair,
          pkNewAddress.toAddress,
          IssuedAsset(ByteStr.decodeBase58(asset).get),
          100000000,
          Waves,
          smartMinFee,
          ByteStr.empty,
          System.currentTimeMillis()
        )
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transfer = sender.signedBroadcast(
      TransferTransaction
        .selfSigned(2.toByte, pkNewAddress, pkNewAddress.toAddress, Waves, 1.waves, Waves, smartMinFee, ByteStr.empty, System.currentTimeMillis())
        .explicitGet()
        .json()
    )
    nodes.waitForHeightAriseAndTxPresent(transfer.id)

    val udpatedScript =
      s"""
         |match tx {
         |  case _: SetScriptTransaction => true
         |  case _ => assetBalance(tx.sender, base58'$asset') >= 900000000 && wavesBalance(tx.sender) >500000000
         |}
      """.stripMargin

    val updated = ScriptCompiler.compile(udpatedScript, estimator).explicitGet()._1

    val updTx =
      sender.signedBroadcast(
        SetScriptTransaction
          .selfSigned(1.toByte, pkNewAddress, Some(updated), setScriptFee + smartFee, System.currentTimeMillis())
          .explicitGet()
          .json()
      )
    nodes.waitForHeightAriseAndTxPresent(updTx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransaction
          .selfSigned(2.toByte, pkNewAddress, pkNewAddress.toAddress, Waves, 1.waves, Waves, smartMinFee, ByteStr.empty, System.currentTimeMillis())
          .explicitGet()
          .json()
      ),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransaction
        .selfSigned(
          2.toByte,
          firstKeyPair,
          pkNewAddress.toAddress,
          IssuedAsset(ByteStr.decodeBase58(asset).get),
          800000000,
          Waves,
          smartMinFee,
          ByteStr.empty,
          System.currentTimeMillis()
        )
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transferAfterUpd = sender.signedBroadcast(
      TransferTransaction
        .selfSigned(2.toByte, pkNewAddress, pkNewAddress.toAddress, Waves, 1.waves, Waves, smartMinFee, ByteStr.empty, System.currentTimeMillis())
        .explicitGet()
        .json()
    )
    nodes.waitForHeightAriseAndTxPresent(transferAfterUpd.id)
  }

  test("split around empty string") {
    val scriptText =
      s"""
         |  {-# STDLIB_VERSION 3       #-}
         |  {-# CONTENT_TYPE   DAPP    #-}
         |  {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         |  @Verifier(tx)
         |  func verify() = {
         |    let strs = split("some", "")
         |    strs.size() == 4  &&
         |    strs[0] == "s"    &&
         |    strs[1] == "o"    &&
         |    strs[2] == "m"    &&
         |    strs[3] == "e"
         |  }
      """.stripMargin

    val compiledScript = ScriptCompiler.compile(scriptText, estimator).explicitGet()._1

    val newAddress   = sender.createKeyPair()
    val pkNewAddress = newAddress
    sender.transfer(firstKeyPair, newAddress.toAddress.toString, 10.waves, minFee, waitForTx = true)

    val scriptSet          = SetScriptTransaction.selfSigned(1.toByte, pkNewAddress, Some(compiledScript), setScriptFee, System.currentTimeMillis())
    val scriptSetBroadcast = sender.signedBroadcast(scriptSet.explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(scriptSetBroadcast.id)

    val transfer = TransferTransaction.selfSigned(
      2.toByte,
      pkNewAddress,
      pkNewAddress.toAddress,
      Waves,
      1.waves,
      Waves,
      smartMinFee,
      ByteStr.empty,
      System.currentTimeMillis()
    )
    val transferBroadcast = sender.signedBroadcast(transfer.explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(transferBroadcast.id)
  }

  test("lastBlock and blockInfoByHeight(last) must return liquid block") {
    val script = ScriptCompiler
      .compile(
        s"""
           |  {-# STDLIB_VERSION 3       #-}
           |  {-# CONTENT_TYPE   DAPP    #-}
           |  {-# SCRIPT_TYPE    ACCOUNT #-}
           |
           |  @Verifier(tx)
           |  func verify() = {
           |    let block = extract(blockInfoByHeight(height))
           |
           |    let checkTs = lastBlock.timestamp == block.timestamp
           |    let checkHeight = block.height == height
           |    let checkHeightLast = lastBlock.height == height
           |    checkTs && checkHeight && checkHeightLast
           |  }
      """.stripMargin,
        estimator
      )
      .explicitGet()
      ._1

    val newAddress = sender.createKeyPair()
    sender.transfer(firstKeyPair, newAddress.toAddress.toString, 10.waves, minFee, waitForTx = true)

    val setScript = sender.setScript(newAddress, Some(script.bytes().base64), setScriptFee)
    nodes.waitForHeightAriseAndTxPresent(setScript.id)

    val transfer = sender.transfer(newAddress, newAddress.toAddress.toString, 1.waves, minFee + (2 * smartFee))
    nodes.waitForHeightAriseAndTxPresent(transfer.id)
  }
}
