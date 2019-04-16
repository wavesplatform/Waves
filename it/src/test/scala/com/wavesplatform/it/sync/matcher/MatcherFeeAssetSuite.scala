package com.wavesplatform.it.sync.matcher

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync.matcher.config.MatcherDefaultConfig.Configs
import com.wavesplatform.it.sync._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.concurrent.duration._

class MatcherFeeAssetSuite extends MatcherSuiteBase with NTPTime {

  import MatcherFeeAssetSuite._

  override protected def nodeConfigs: Seq[Config] = Configs.map(configWithOrderFeeFixed().withFallback(_))

  private def matcher = dockerNodes().head

  val aliceAssetBase58: String = aliceNode
    .issue(
      sourceAddress = aliceAcc.address,
      name = "AliceCoin",
      description = "AliceCoin for matcher's tests",
      quantity = someAssetAmount,
      decimals = 0,
      reissuable = false,
      fee = smartIssueFee)
    .id
  val aliceAsset = IssuedAsset(ByteStr.decodeBase58(aliceAssetBase58).get)
  val aliceScriptedAssetBase58: String = aliceNode
    .issue(
      sourceAddress = aliceAcc.address,
      name = "AliceSmartAsset",
      description = "AliceSmartAsset for matcher's tests",
      quantity = someAssetAmount,
      decimals = 0,
      reissuable = false,
      fee = smartIssueFee,
      script = Some(scriptBase64))
    .id
  val aliceScriptedAsset = IssuedAsset(ByteStr.decodeBase58(aliceScriptedAssetBase58).get)
  Seq(aliceAssetBase58, aliceScriptedAssetBase58).foreach(matcherNode.waitForTransaction(_))

  "Matcher" - {
    "when has some asset as fixed fee in config and orders placed" - {
      "should accept orders if orders' matcherFeeAsset equal to specified in config" in {
        for ((fixedAsset, matcherFeeOrder1, matcherFeeOrder2) <- Seq(
          (aliceAssetBase58, aliceAsset, aliceAsset),
          (aliceScriptedAssetBase58, aliceScriptedAsset, aliceScriptedAsset)
        )) {

          docker.restartNode(matcher,
            configWithOrderFeeFixed(
              matcherFeeAssetId = fixedAsset
            ))

          val aliceWavesPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(fixedAsset).get), Waves)

          aliceNode.assertAssetBalance(aliceAcc.address, fixedAsset, someAssetAmount)

          val transfer1ToBobId = aliceNode.transfer(aliceAcc.address, bobAcc.address, someAssetAmount / 2, minFee, Some(fixedAsset), None, 2).id
          matcherNode.waitForTransaction(transfer1ToBobId)

          matcherNode.assertAssetBalance(bobAcc.address, fixedAsset, someAssetAmount / 2)

          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount = 1
          val buy = Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts, expirationTimestamp, matcherFee, version = 3, matcherFeeOrder1)
          val sell = Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts, expirationTimestamp, matcherFee, version = 3, matcherFeeOrder2)

          val aliceOrderIdFill = matcherNode
            .placeOrder(buy)
            .message
            .id
          val bobSellOrderId = matcherNode
            .placeOrder(sell)
            .message
            .id

          orderStatus(aliceAcc, aliceWavesPair, aliceOrderIdFill, "Filled")
          orderStatus(bobAcc, aliceWavesPair, bobSellOrderId, "Filled")
        }
      }

      "should decline orders if orders' matcherFeeAsset not equal to specified in config" in {
        val ts = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime
        val amount = 1
        val aliceWavesPair = AssetPair(aliceAsset, Waves)
        val buy = Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts, expirationTimestamp, matcherFee, version = 3, Waves)

        assertBadRequestAndResponse(matcherNode
          .placeOrder(buy), f"Required $aliceScriptedAssetBase58 as asset fee, but given WAVES")
      }
    }
    "when has percent-mode for fee in config and orders placed" - {
      "should accept orders with amount/price/spending/receiving assets as matcherFeeAsset" in {
        for (percentAssetType <- Seq(
          "amount",
          "price",
          "spending",
          "receiving"
        )) {
          docker.restartNode(matcher,
            configWithOrderFeePercent(
              assetType = percentAssetType
            ))
          for (assetInPair <- Seq(
            aliceAsset,
            aliceScriptedAsset
          )) {
            val aliceWavesPair = AssetPair(assetInPair, Waves)
            val ts = ntpTime.correctedTime()
            val expirationTimestamp = ts + Order.MaxLiveTime - 10000
            val amount = 100
            var buy = None: Option[Order]
            var sell = None: Option[Order]

            percentAssetType match {
              case "amount" =>
                buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, assetInPair))
                sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, assetInPair))
              case "price" =>
                buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, Waves))
                sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, Waves))
              case "spending" =>
                buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, Waves))
                sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, assetInPair))
              case "receiving" =>
                buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, assetInPair))
                sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                  expirationTimestamp, matcherFee, version = 3, Waves))
            }
            val aliceOrderIdFill = matcherNode
              .placeOrder(buy.get)
              .message
              .id
            val bobOrderIdFill = matcherNode
              .placeOrder(sell.get)
              .message
              .id

            orderStatus(aliceAcc, aliceWavesPair, aliceOrderIdFill, "Filled")
            orderStatus(bobAcc, aliceWavesPair, bobOrderIdFill, "Filled")

          }
        }
      }

      "should decline orders if matcherFeeAsset not equal to amount/price/spending/receiving assets" in {
        for (percentAssetType <- Seq(
          "amount",
          "price",
          "spending",
          "receiving"
        )) {
          docker.restartNode(matcher,
            configWithOrderFeePercent(
              assetType = percentAssetType
            ))
          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount = 100
          val aliceWavesPair = AssetPair(aliceAsset, Waves)
          var buy = None: Option[Order]
          var sell = None: Option[Order]
          var requiredBuyMatcherFeeAsset = None: Option[String]
          var requiredSellMatcherFeeAsset = None: Option[String]

          percentAssetType match {
            case "amount" =>
              buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, Waves))
              sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, Waves))
              requiredBuyMatcherFeeAsset= Some(aliceAssetBase58)
              requiredSellMatcherFeeAsset = Some(aliceAssetBase58)
            case "price" =>
              buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset))
              sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset))
              requiredBuyMatcherFeeAsset = Some("WAVES")
              requiredSellMatcherFeeAsset = Some("WAVES")
            case "spending" =>
              buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset))
              sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, Waves))
              requiredBuyMatcherFeeAsset = Some("WAVES")
              requiredSellMatcherFeeAsset = Some(aliceAssetBase58)
            case "receiving" =>
              buy = Some(Order.buy(aliceAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, Waves))
              sell = Some(Order.sell(bobAcc, matcherNode.privateKey.publicKey, aliceWavesPair, amount, Order.PriceConstant, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset))
              requiredBuyMatcherFeeAsset = Some(aliceAssetBase58)
              requiredSellMatcherFeeAsset = Some("WAVES")
          }

          assertBadRequestAndResponse(matcherNode
            .placeOrder(buy.get), f"Required ${requiredBuyMatcherFeeAsset.get} as asset fee")
          assertBadRequestAndResponse(matcherNode
            .placeOrder(sell.get), f"Required ${requiredSellMatcherFeeAsset.get} as asset fee")

        }
      }
    }
  }

  private def orderStatus(sender: KeyPair, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    matcherNode.waitOrderStatus(assetPair, orderId, expectedStatus, waitTime = 2.minutes)
}

object MatcherFeeAssetSuite {

  def configWithOrderFeeFixed(matcherFeeAssetId: String = "WAVES"): Config = {
    parseString(
      s"""
         |waves.matcher {
         | allow-order-v3 = yes
         |  order-fee {
         |    mode = "fixed"
         |
         |    fixed {
         |      asset = "$matcherFeeAssetId"
         |      min-fee = 300000
         |   }
         |  }
         |}""".stripMargin)
  }

  def configWithOrderFeePercent(assetType: String): Config = {
    parseString(
      s"""
         |waves.matcher {
         | allow-order-v3 = yes
         |  order-fee {
         |    mode = "percent"
         |
         |    percent {
         |        asset-type = "$assetType"
         |
         |        min-fee = 0.1
         |      }
         |  }
         |}""".stripMargin)
  }

}
