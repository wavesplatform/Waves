package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.{MatcherSuiteBase, NTPTime}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{ExchangeTransaction, SyncMatcherHttpApi}
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import play.api.libs.json.Json.parse

import scala.concurrent.duration._

class MatcherFeeAssetSuite extends MatcherSuiteBase with NTPTime {

  import MatcherFeeAssetSuite._

  override protected def nodeConfigs: Seq[Config] = Seq(configWithOrderFeeFixed().withFallback(Configs.head))//Configs.map(configWithOrderFeeFixed().withFallback(_))

  val price = 100000000L

  val aliceAssetBase58: String = node
    .issue(
      sourceAddress = alice.address,
      name = "AliceCoin",
      description = "AliceCoin for matcher's tests",
      quantity = someAssetAmount,
      decimals = 0,
      reissuable = false,
      fee = smartIssueFee)
    .id
  val aliceAsset = IssuedAsset(ByteStr.decodeBase58(aliceAssetBase58).get)
  val aliceScriptedAssetBase58: String = node
    .issue(
      sourceAddress = alice.address,
      name = "AliceSmartAsset",
      description = "AliceSmartAsset for matcher's tests",
      quantity = someAssetAmount,
      decimals = 0,
      reissuable = false,
      fee = smartIssueFee,
      script = Some(scriptBase64))
    .id
  val aliceScriptedAsset = IssuedAsset(ByteStr.decodeBase58(aliceScriptedAssetBase58).get)
  Seq(aliceAssetBase58, aliceScriptedAssetBase58).foreach(node.waitForTransaction(_))

  val matcherPublicKey = matcher.publicKey

  "Matcher" - {
    "when has some asset as fixed fee in config and orders placed" - {
      "should accept orders if orders' matcherFeeAsset equal to specified in config" in {
        for (fixedAsset <- Seq(
          aliceAsset,
          aliceScriptedAsset
        )) {
          val fixedAssetBase58 = fixedAsset.id.base58
          docker.restartNode(node,
            configWithOrderFeeFixed(
              matcherFeeAssetId = fixedAssetBase58
            ))

          val aliceWavesPair = AssetPair(fixedAsset, Waves)

          val transfer1ToBobId = node.transfer(alice.address, bob.address, someAssetAmount / 2, minFee, Some(fixedAssetBase58), None, 2).id
          node.waitForTransaction(transfer1ToBobId)

          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount = 1
          val aliceAssetBalanceBefore = node.assetBalance(alice.address, fixedAssetBase58).balance
          val bobAssetBalanceBefore = node.assetBalance(bob.address, fixedAssetBase58).balance

          val aliceOrderIdFill = node
            .placeOrder(Order
              .buy(
                sender = alice,
                matcher = matcherPublicKey,
                pair = aliceWavesPair,
                amount = amount,
                price = price,
                timestamp = ts,
                expiration = expirationTimestamp,
                matcherFee = matcherFee,
                version = 3,
                matcherFeeAssetId = fixedAsset))
            .message
            .id
          val bobSellOrderId = node
            .placeOrder(Order
              .sell(
                sender = bob,
                matcher = matcherPublicKey,
                pair = aliceWavesPair,
                amount = amount,
                price = price,
                timestamp = ts,
                expiration = expirationTimestamp,
                matcherFee = matcherFee,
                version = 3,
                matcherFeeAssetId = fixedAsset))
            .message
            .id

          orderStatus(alice, aliceWavesPair, aliceOrderIdFill, "Filled")
          orderStatus(bob, aliceWavesPair, bobSellOrderId, "Filled")

          node.waitOrderInBlockchain(aliceOrderIdFill)

          node.assetBalance(alice.address, fixedAssetBase58).balance shouldBe aliceAssetBalanceBefore - matcherFee + amount
          node.assetBalance(bob.address, fixedAssetBase58).balance shouldBe bobAssetBalanceBefore - matcherFee - amount

        }
      }

      "should decline orders if orders' matcherFeeAsset not equal to specified in config" in {
        val ts = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime
        val amount = 1
        val aliceWavesPair = AssetPair(aliceAsset, Waves)
        val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, Waves)

        assertBadRequestAndResponse(node
          .placeOrder(buy), f"Required $aliceScriptedAssetBase58 as asset fee, but given WAVES")
      }
    }
    "when has percent-mode for fee in config and orders placed" - {
      "should accept orders with amount/price/spending/receiving assets as matcherFeeAsset" in {
        val minFeePercent = 0.1
        for (percentAssetType <- Seq(
          "amount",
          "price",
          "spending",
          "receiving"
        )) {
          docker.restartNode(node,
            configWithOrderFeePercent(
              assetType = percentAssetType,
              minFeePercent = minFeePercent
            ))
          for (assetInPair <- Seq(
            aliceAsset,
            aliceScriptedAsset
          )) {
            val assetInPairBase58 = assetInPair.id.base58
            val aliceWavesPair = AssetPair(assetInPair, Waves)
            val ts = ntpTime.correctedTime()
            val expirationTimestamp = ts + Order.MaxLiveTime - 10000
            val amount = 100

            val (buy, sell, buyMatcherFeeAsset, sellMatcherFeeAsset) = percentAssetType match {
              case "amount" =>
                val buyMatcherFeeAsset = assetInPair
                val sellMatcherFeeAsset = assetInPair
                val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, buyMatcherFeeAsset)
                val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, sellMatcherFeeAsset)
                (buy, sell, buyMatcherFeeAsset, sellMatcherFeeAsset)
              case "price" =>
                val buyMatcherFeeAsset = Waves
                val sellMatcherFeeAsset = Waves
                val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, buyMatcherFeeAsset)
                val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, sellMatcherFeeAsset)
                (buy, sell, buyMatcherFeeAsset, sellMatcherFeeAsset)
              case "spending" =>
                val buyMatcherFeeAsset = Waves
                val sellMatcherFeeAsset = assetInPair
                val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, buyMatcherFeeAsset)
                val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, sellMatcherFeeAsset)
                (buy, sell, buyMatcherFeeAsset, sellMatcherFeeAsset)
              case "receiving" =>
                val buyMatcherFeeAsset = assetInPair
                val sellMatcherFeeAsset = Waves
                val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, buyMatcherFeeAsset)
                val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                  expirationTimestamp, matcherFee, version = 3, sellMatcherFeeAsset)
                (buy, sell, buyMatcherFeeAsset, sellMatcherFeeAsset)
            }
            val aliceWavesBalanceBefore = node.balanceDetails(alice.address).available
            val bobWavesBalanceBefore = node.balanceDetails(bob.address).available
            val aliceAssetBalanceBefore = node.assetBalance(alice.address, assetInPairBase58).balance
            val bobAssetBalanceBefore = node.assetBalance(bob.address, assetInPairBase58).balance

            val aliceOrderIdFill = node
              .placeOrder(buy)
              .message
              .id
            val bobOrderIdFill = node
              .placeOrder(sell)
              .message
              .id

            orderStatus(alice, aliceWavesPair, aliceOrderIdFill, "Filled")
            orderStatus(bob, aliceWavesPair, bobOrderIdFill, "Filled")

            node.waitOrderInBlockchain(aliceOrderIdFill)

            (buyMatcherFeeAsset, sellMatcherFeeAsset) match {
              case (IssuedAsset(_), IssuedAsset(_)) =>
                node.assetBalance(alice.address, assetInPairBase58).balance shouldBe aliceAssetBalanceBefore - matcherFee + amount
                node.assetBalance(bob.address, assetInPairBase58).balance shouldBe bobAssetBalanceBefore - matcherFee - amount
              case (Waves, Waves) =>
                node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - matcherFee - amount
                node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore - matcherFee + amount
              case (Waves, IssuedAsset(_)) =>
                node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - matcherFee - amount
                node.assetBalance(bob.address, assetInPairBase58).balance shouldBe bobAssetBalanceBefore - matcherFee - amount
              case (IssuedAsset(_), Waves) =>
                node.assetBalance(alice.address, assetInPairBase58).balance shouldBe aliceAssetBalanceBefore - matcherFee + amount
                node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore - matcherFee + amount
            }
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
          docker.restartNode(node,
            configWithOrderFeePercent(
              assetType = percentAssetType,
              minFeePercent = 0.1
            ))
          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount = 100
          val aliceWavesPair = AssetPair(aliceAsset, Waves)

          val (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset) = percentAssetType match {
            case "amount" =>
              val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, Waves)
              val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, Waves)
              val requiredBuyMatcherFeeAsset = aliceAssetBase58
              val requiredSellMatcherFeeAsset = aliceAssetBase58
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "price" =>
              val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val requiredBuyMatcherFeeAsset = "WAVES"
              val requiredSellMatcherFeeAsset = "WAVES"
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "spending" =>
              val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, Waves)
              val requiredBuyMatcherFeeAsset = "WAVES"
              val requiredSellMatcherFeeAsset = aliceAssetBase58
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "receiving" =>
              val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, Waves)
              val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts,
                expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val requiredBuyMatcherFeeAsset = aliceAssetBase58
              val requiredSellMatcherFeeAsset = "WAVES"
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
          }

          assertBadRequestAndResponse(node
            .placeOrder(buy), f"Required $requiredBuyMatcherFeeAsset as asset fee")
          assertBadRequestAndResponse(node
            .placeOrder(sell), f"Required $requiredSellMatcherFeeAsset as asset fee")

        }
      }
    }
    "when has insufficient amount of WAVES to pay ExchangeTransaction fee and place orders" - {
      "generated ExchangeTransaction should not appears in blockchain" in {
        docker.restartNode(node,
          configWithOrderFeeFixed(
            matcherFeeAssetId = aliceAssetBase58))
        val matcherWavesBalance = node.balanceDetails(node.address).available
        node.transfer(node.address, alice.address, matcherWavesBalance - minFee, minFee, version = 2, waitForTx = true)

        val ts = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime - 10000
        val amount = 100
        val aliceWavesPair = AssetPair(aliceAsset, Waves)

        val aliceOrderIdFill = node
          .placeOrder(Order
            .buy(
              sender = alice,
              matcher = matcherPublicKey,
              pair = aliceWavesPair,
              amount = amount,
              price = price,
              timestamp = ts,
              expiration = expirationTimestamp,
              matcherFee = matcherFee,
              version = 3,
              matcherFeeAssetId = aliceAsset))
          .message
          .id

        node.placeOrder(Order
            .sell(
              sender = bob,
              matcher = matcherPublicKey,
              pair = aliceWavesPair,
              amount = amount,
              price = price,
              timestamp = ts,
              expiration = expirationTimestamp,
              matcherFee = matcherFee,
              version = 3,
              matcherFeeAssetId = aliceAsset))

        nodes.waitForHeightArise()

        val transactionByOrderResponse = node.matcherGet(s"/matcher/transactions/$aliceOrderIdFill")
        val exchangeTxId = parse(transactionByOrderResponse.getResponseBody(StandardCharsets.UTF_8)).as[ExchangeTransaction].id

        SyncMatcherHttpApi.assertNotFoundAndMessage(node.transactionInfo(exchangeTxId),
          "Transaction is not in blockchain")
      }
    }
  }

  private def orderStatus(sender: KeyPair, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    node.waitOrderStatus(assetPair, orderId, expectedStatus, waitTime = 2.minutes)

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

  def configWithOrderFeePercent(assetType: String, minFeePercent: Double): Config = {
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
         |        min-fee = "$minFeePercent"
         |      }
         |  }
         |}""".stripMargin)
  }

}

