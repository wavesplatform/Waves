package com.wavesplatform.it.sync.matcher.config

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.api.http.assets.SignedIssueV1Request
import com.wavesplatform.it.sync._
import com.wavesplatform.matcher.market.MatcherActor
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.AssetPair

import scala.util.Random

object MatcherDefaultConfig {

  import ConfigFactory._
  import com.wavesplatform.it.NodeConfigs._

  val ForbiddenAssetId = "FdbnAsset"
  val orderLimit       = 20

  val minerEnabled  = parseString(s"""
       |waves.miner.enable = yes
       |waves.miner.quorum = 0""".stripMargin)
  val minerDisabled = parseString("waves.miner.enable = no")
  val matcherConfig = parseString(s"""
                                     |waves.miner.enable = no
                                     |waves.matcher {
                                     |  enable = yes
                                     |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                     |  bind-address = "0.0.0.0"
                                     |  blacklisted-assets = ["$ForbiddenAssetId"]
                                     |  balance-watching.enable = yes
                                     |  rest-order-limit=$orderLimit
                                     |}""".stripMargin)

  val Configs: Seq[Config] = List(9, 5, 7)
    .map(Default)
    .zip(Seq(matcherConfig, minerDisabled, minerEnabled))
    .map { case (n, o) => o.withFallback(n) }

  def issueAssetPair(issuer: PrivateKeyAccount,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (SignedIssueV1Request, SignedIssueV1Request, AssetPair) = {
    issueAssetPair(issuer, issuer, amountAssetDecimals, priceAssetDecimals)
  }

  def issueAssetPair(amountAssetIssuer: PrivateKeyAccount,
                     priceAssetIssuer: PrivateKeyAccount,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (SignedIssueV1Request, SignedIssueV1Request, AssetPair) = {

    val issueAmountAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = amountAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = amountAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = priceAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), Some(issueAmountAssetTx.id().arr)) < 0) {
      (createSignedIssueRequest(issueAmountAssetTx),
       createSignedIssueRequest(issuePriceAssetTx),
       AssetPair(
         amountAsset = IssuedAsset(issueAmountAssetTx.id()),
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      issueAssetPair(amountAssetIssuer, priceAssetIssuer, amountAssetDecimals, priceAssetDecimals)
  }

  def assetPairIssuePriceAsset(issuer: PrivateKeyAccount, amountAssetId: Asset, priceAssetDecimals: Byte): (SignedIssueV1Request, AssetPair) = {

    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = issuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), amountAssetId.compatId.map(_.arr)) < 0) {
      (createSignedIssueRequest(issuePriceAssetTx),
       AssetPair(
         amountAsset = amountAssetId,
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      assetPairIssuePriceAsset(issuer, amountAssetId, priceAssetDecimals)
  }

}
