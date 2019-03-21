package com.wavesplatform.it.sync.config

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.config.ConfigFactory.{empty, parseString}
import com.wavesplatform.account.{AddressScheme, PrivateKeyAccount}
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.wavesplatform.it.sync.config.MatcherDefaultConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.AssetPairBuilder
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.wallet.Wallet

import scala.util.Random
import scala.collection.JavaConverters._

// TODO: Make it trait
object MatcherPriceAssetConfig {

  private val Default: Config = ConfigFactory.parseResources("nodes.conf")

  val accounts: Map[String, PrivateKeyAccount] = {
    val config           = ConfigFactory.parseResources("genesis.conf")
    val distributionsKey = "genesis-generator.distributions"
    val distributions    = config.getObject(distributionsKey)
    distributions
      .keySet()
      .asScala
      .map { accountName =>
        val prefix   = s"$distributionsKey.$accountName"
        val seedText = config.getString(s"$prefix.seed-text")
        val nonce    = config.getInt(s"$prefix.nonce")
        accountName -> Wallet.generateNewAccount(seedText.getBytes(StandardCharsets.UTF_8), nonce)
      }
      .toMap
  }

//  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(2))
//    .zip(Seq(matcherConfig.withFallback(minerDisabled), minerDisabled, empty()))
//    .map { case (n, o) => o.withFallback(n) }

  val matcher: PrivateKeyAccount = accounts("matcher")
  val alice: PrivateKeyAccount   = accounts("alice")
  val bob: PrivateKeyAccount     = accounts("bob")

  val Decimals: Byte = 2

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val ethAssetName = "ETH-X"
  val btcAssetName = "BTC-X"

  val IssueUsdTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueEthTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = ethAssetName.getBytes(),
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

  val IssueBtcTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = btcAssetName.getBytes(),
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
    amountAsset = IssuedAsset(WctId),
    priceAsset = IssuedAsset(UsdId)
  )

  val wctWavesPair = AssetPair(
    amountAsset = IssuedAsset(WctId),
    priceAsset = Waves
  )

  val ethWavesPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = Waves
  )

  val ethBtcPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = IssuedAsset(BtcId)
  )

  val wavesUsdPair = AssetPair(
    amountAsset = Waves,
    priceAsset = IssuedAsset(UsdId)
  )

  val ethUsdPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = IssuedAsset(UsdId)
  )

  val wavesBtcPair = AssetPair(
    amountAsset = Waves,
    priceAsset = IssuedAsset(BtcId)
  )

  val orderLimit = 10

  val updatedMatcherConfig = parseString(s"""waves.matcher {
                                                    |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
                                                    |  rest-order-limit = $orderLimit
                                                    |}""".stripMargin)

  val Configs: Seq[Config] = Seq.empty // _Configs.map(updatedMatcherConfig.withFallback(_))

  def createAssetPair(asset1: String, asset2: String): AssetPair = {
    val (a1, a2) = (AssetPair.extractAssetId(asset1).get, AssetPair.extractAssetId(asset2).get)
    if (AssetPairBuilder.assetIdOrdering.compare(a1.compatId, a2.compatId) > 0)
      AssetPair(a1, a2)
    else
      AssetPair(a2, a1)
  }

}
