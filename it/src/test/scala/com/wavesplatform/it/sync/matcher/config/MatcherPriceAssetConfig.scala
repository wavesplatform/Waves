package com.wavesplatform.it.sync.matcher.config

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.{empty, parseString}
import com.wavesplatform.account.{AddressScheme, PrivateKeyAccount}
import com.wavesplatform.api.http.assets.SignedIssueV2Request
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.Base58

import scala.util.Random

object MatcherPriceAssetConfig {

  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

  private val aliceSeed = _Configs(1).getString("account-seed")
  private val bobSeed   = _Configs(2).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  private val bobPk     = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val Decimals: Byte = 2

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val ethAssetName = "ETH-X"

  val IssueUsdTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      2,
      AddressScheme.current.chainId,
      sender = alicePk,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      2,
      AddressScheme.current.chainId,
      sender = bobPk,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueEthTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      2,
      AddressScheme.current.chainId,
      sender = alicePk,
      name = ethAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueBtcTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      2,
      AddressScheme.current.chainId,
      sender = bobPk,
      name = "BTC-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val BtcId = IssueBtcTx.id()
  val EthId = IssueEthTx.id()
  val UsdId = IssueUsdTx.id()
  val WctId = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = Some(UsdId)
  )

  val wctWavesPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = None
  )

  val ethWavesPair = AssetPair(
    amountAsset = Some(EthId),
    priceAsset = None
  )

  val ethBtcPair = AssetPair(
    amountAsset = Some(EthId),
    priceAsset = Some(BtcId)
  )

  val wavesUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  val ethUsdPair = AssetPair(
    amountAsset = Some(EthId),
    priceAsset = Some(UsdId)
  )

  val orderLimit = 10

  private val updatedMatcherConfig = parseString(s"""
                                                    |waves.matcher {
                                                    |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
                                                    |  rest-order-limit=$orderLimit
                                                    |}
     """.stripMargin)

  val Configs: Seq[Config] = _Configs.map(updatedMatcherConfig.withFallback(_))

  def createSignedIssueRequest(tx: IssueTransactionV2): SignedIssueV2Request = {
    import tx._
    SignedIssueV2Request(
      2,
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      tx.proofs.base58().toList
    )
  }

}
