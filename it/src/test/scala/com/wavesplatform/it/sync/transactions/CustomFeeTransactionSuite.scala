package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import scorex.account.PrivateKeyAccount
import scorex.api.http.assets.SignedIssueRequest
import scorex.crypto.encode.Base58
import scorex.transaction.assets.IssueTransaction

class CustomFeeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  import CustomFeeTransactionSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val defaultAssetQuantity = 1000000
  private val transferFee          = 100000
  private val issueFee             = 1.waves

  test("make transfer with custom asset") {
    val (balance1, eff1) = notMiner.accountBalances(addressDefaultNode)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val req           = createSignedIssueRequest(assetTx)
    val issuedAssetId = sender.signedIssue(req).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    notMiner.assertBalances(addressDefaultNode, balance1 - issueFee, eff1 - issueFee)
    notMiner.assertAssetBalance(addressDefaultNode, issuedAssetId, defaultAssetQuantity)

    val transferTransactionId = sender.transfer(addressDefaultNode, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTransactionId)
    notMiner.assertBalances(addressDefaultNode, balance1 - issueFee, eff1 - issueFee)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertAssetBalance(addressDefaultNode, issuedAssetId, 1000000 - transferFee - 1)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 1)

  }

}

object CustomFeeTransactionSuite {
  val addressDefaultNode = Default(3).getString("address")
  private val seed       = Default(3).getString("account-seed")
  private val pk         = PrivateKeyAccount.fromSeed(seed).right.get
  val assetTx = IssueTransaction
    .create(
      sender = pk,
      name = "asset".getBytes(),
      description = "asset description".getBytes(),
      quantity = 1000000,
      decimals = 2,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val assetId = assetTx.id()

  private val acceptAssetsFee = ConfigFactory.parseString(s"waves.fees.transfer.$assetId = 100000")

  private val notMinerConfig = ConfigFactory.parseString("waves.miner.enable=no").withFallback(acceptAssetsFee)

  val Configs: Seq[Config] = Seq(
    acceptAssetsFee.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1)),
    notMinerConfig.withFallback(Default(2)),
    notMinerConfig.withFallback(Default(3))
  )

  def createSignedIssueRequest(tx: IssueTransaction): SignedIssueRequest = {
    import tx._
    SignedIssueRequest(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }
}
