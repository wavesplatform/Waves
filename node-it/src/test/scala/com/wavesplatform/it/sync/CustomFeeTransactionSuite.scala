package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.test._
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.utils._
import org.scalatest.CancelAfterFailure

class CustomFeeTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  import CustomFeeTransactionSuite._

  override protected def nodeConfigs: Seq[Config] = Configs

  private val transferFee = 100000
  private val assetFee    = 1.waves
  private val assetToken  = 100

  test("make transfer with sponsored asset") {
    val (balance1, eff1) = notMiner.accountBalances(senderAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)
    val (balance3, eff3) = notMiner.accountBalances(minerAddress)

    val req           = createIssueRequest(assetTx)
    val issuedAssetId = notMiner.signedIssue(req).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    val sponsorAssetId = notMiner.sponsorAsset(senderKeyPair, issuedAssetId, assetToken, assetFee).id
    assert(sponsorAssetId.nonEmpty)
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val fees = 2 * assetFee
    notMiner.assertBalances(senderAddress, balance1 - fees, eff1 - fees)
    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity)

    // until `feature-check-blocks-period` blocks have been mined, sponsorship does not occur
    val unsponsoredId =
      notMiner.transfer(senderKeyPair, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(unsponsoredId)
    notMiner.assertBalances(senderAddress, balance1 - fees, eff1 - fees)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertBalances(minerAddress, balance3 + fees, eff3 + fees)

    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity - transferFee - 1)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 1)
    notMiner.assertAssetBalance(minerAddress, issuedAssetId, transferFee)

    // after `feature-check-blocks-period` asset fees should be sponsored
    nodes.waitForSameBlockHeadersAt(featureCheckBlocksPeriod)
    val sponsoredId = notMiner.transfer(senderKeyPair, secondAddress, 1, transferFee, Some(issuedAssetId), Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(sponsoredId)

    val sponsorship = Sponsorship.toWaves(transferFee, assetToken)
    notMiner.assertBalances(senderAddress, balance1 - fees - sponsorship, eff1 - fees - sponsorship)
    notMiner.assertBalances(secondAddress, balance2, eff2)
    notMiner.assertBalances(minerAddress, balance3 + fees + sponsorship, balance3 + fees + sponsorship)

    notMiner.assertAssetBalance(senderAddress, issuedAssetId, defaultAssetQuantity - transferFee - 2)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, 2)
    notMiner.assertAssetBalance(minerAddress, issuedAssetId, transferFee)
  }

}

object CustomFeeTransactionSuite {
  private val minerAddress             = Default.head.getString("address")
  private val senderAddress            = Default(2).getString("address")
  private val seed                     = Default(2).getString("account-seed")
  private val senderKeyPair            = KeyPair.fromSeed(seed).explicitGet()
  private val defaultAssetQuantity     = 999999999999L
  private val featureCheckBlocksPeriod = 13

  private val assetTx = IssueTransaction(
    TxVersion.V1,
    sender = senderKeyPair.publicKey,
    "asset".utf8Bytes,
    "asset description".utf8Bytes,
    quantity = defaultAssetQuantity,
    decimals = 2,
    reissuable = false,
    script = None,
    fee = 1.waves,
    timestamp = System.currentTimeMillis()
  ).signWith(senderKeyPair.privateKey)

  private val assetId = assetTx.id()

  private val minerConfig = ConfigFactory.parseString(s"""waves.fees.transfer.$assetId = 100000
                                                         |waves.blockchain.custom.functionality {
                                                         |  feature-check-blocks-period = $featureCheckBlocksPeriod
                                                         |  blocks-for-feature-activation = $featureCheckBlocksPeriod
                                                         |  pre-activated-features = { 7 = 0, 14 = 1000000 }
                                                         |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString("waves.miner.enable=no").withFallback(minerConfig)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1)),
    notMinerConfig.withFallback(Default(2))
  )

}
