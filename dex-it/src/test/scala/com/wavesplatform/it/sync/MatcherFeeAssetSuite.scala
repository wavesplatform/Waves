package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.it.{MatcherSuiteBase, NTPTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.concurrent.duration._

class MatcherFeeAssetSuite extends MatcherSuiteBase with NTPTime {

  import MatcherFeeAssetSuite._

  override protected def nodeConfigs: Seq[Config] =
    Seq(configWithOrderFeeDynamic(minMatcherFee).withFallback(Configs.head))

  val minMatcherFee = 200000L
  val price         = 100000000L

  val aliceAssetBase58: String = node
    .broadcastIssue(
      alice,
      name = "AliceCoin",
      description = "AliceCoin for matcher's tests",
      quantity = someAssetAmount,
      decimals = 0,
      reissuable = false,
      fee = smartIssueFee,
      script = None,
      waitForTx = true
    )
    .id
  val aliceAsset = IssuedAsset(ByteStr.decodeBase58(aliceAssetBase58).get)
  val aliceScriptedAssetBase58: String = node
    .broadcastIssue(
      alice,
      name = "AliceSmartAsset",
      description = "AliceSmartAsset for matcher's tests",
      quantity = someAssetAmount,
      decimals = 0,
      reissuable = false,
      fee = smartIssueFee,
      script = Some(scriptBase64),
      waitForTx = true
    )
    .id
  val aliceScriptedAsset = IssuedAsset(ByteStr.decodeBase58(aliceScriptedAssetBase58).get)

  val matcherPublicKey: PublicKey = matcher.publicKey

  "Matcher" - {

    node.broadcastTransfer(alice, bob.address, someAssetAmount / 2, minFee, Some(aliceAssetBase58), None, waitForTx = true).id
    node.broadcastTransfer(alice, bob.address, someAssetAmount / 2, minFee, Some(aliceScriptedAssetBase58), None, waitForTx = true).id

    "when has dynamic mode in config and orders placed" - {
      "should accept orders if orders' matcherFeeAsset is WAVES and matcherFee amount is equal to specified in config" in {
        val ts                      = ntpTime.correctedTime()
        val expirationTimestamp     = ts + Order.MaxLiveTime - 10000
        val orderAmount             = 1
        val someWavesPair           = AssetPair(aliceAsset, Waves)
        val aliceWavesBalanceBefore = node.balanceDetails(alice.address).available
        val bobWavesBalanceBefore   = node.balanceDetails(bob.address).available
        val aliceAssetBalanceBefore = node.assetBalance(alice.address, aliceAssetBase58).balance
        val bobAssetBalanceBefore   = node.assetBalance(bob.address, aliceAssetBase58).balance

        val aliceBuyOrderId = node
          .placeOrder(
            Order
              .buy(
                sender = alice,
                matcher = matcherPublicKey,
                pair = someWavesPair,
                amount = orderAmount,
                price = price,
                timestamp = ts,
                expiration = expirationTimestamp,
                matcherFee = minMatcherFee,
                version = 3,
                matcherFeeAssetId = Waves
              ))
          .message
          .id
        val bobSellOrderId = node
          .placeOrder(
            Order
              .sell(
                sender = bob,
                matcher = matcherPublicKey,
                pair = someWavesPair,
                amount = orderAmount,
                price = price,
                timestamp = ts,
                expiration = expirationTimestamp,
                matcherFee = minMatcherFee,
                version = 3,
                matcherFeeAssetId = Waves
              ))
          .message
          .id

        orderStatus(alice, someWavesPair, aliceBuyOrderId, "Filled")
        orderStatus(bob, someWavesPair, bobSellOrderId, "Filled")

        node.waitOrderInBlockchain(aliceBuyOrderId)

        node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - (orderAmount * price / 100000000) - minMatcherFee
        node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore + (orderAmount * price / 100000000) - minMatcherFee
        node.assetBalance(alice.address, aliceAssetBase58).balance shouldBe aliceAssetBalanceBefore + orderAmount
        node.assetBalance(bob.address, aliceAssetBase58).balance shouldBe bobAssetBalanceBefore - orderAmount

      }
      "should reject orders if orders' matcherFee is less then specified in config" in {
        val ts                     = ntpTime.correctedTime()
        val expirationTimestamp    = ts + Order.MaxLiveTime - 10000
        val orderAmount            = 1
        val someWavesPair          = AssetPair(aliceAsset, Waves)
        val insufficientMatcherFee = minMatcherFee - 1

        assertBadRequestAndMessage(
          node
            .placeOrder(
              Order
                .buy(
                  sender = alice,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = insufficientMatcherFee,
                  version = 3,
                  matcherFeeAssetId = Waves
                )),
          f"Required $minMatcherFee WAVES as fee for this order, but given $insufficientMatcherFee"
        )
        assertBadRequestAndMessage(
          node
            .placeOrder(
              Order
                .sell(
                  sender = bob,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = insufficientMatcherFee,
                  version = 3,
                  matcherFeeAssetId = Waves
                )),
          f"Required $minMatcherFee WAVES as fee for this order, but given $insufficientMatcherFee"
        )
      }
      "should reject orders if orders' matcherFeeAsset is not WAVES" in {
        val ts                  = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime - 10000
        val orderAmount         = 1
        val someWavesPair       = AssetPair(aliceAsset, Waves)

        assertBadRequestAndMessage(
          node.placeOrder(
              Order
                .buy(
                  sender = alice,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = minMatcherFee,
                  version = 3,
                  matcherFeeAssetId = aliceAsset
                )),
          f"Required one of the following: WAVES as asset fee, but given $aliceAssetBase58"
        )
        assertBadRequestAndMessage(
          node.placeOrder(
              Order
                .sell(
                  sender = bob,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = minMatcherFee,
                  version = 3,
                  matcherFeeAssetId = aliceAsset
                )),
          f"Required one of the following: WAVES as asset fee, but given $aliceAssetBase58"
        )
      }
    }

    "when has some asset as fixed fee in config and orders placed" - {
      "should accept orders if orders' matcherFeeAsset equal to specified in config" in {
        for (fixedAssetMatcherFee <- Seq(
               aliceAsset,
               aliceScriptedAsset
             )) {
          val fixedAssetMatcherFeeBase58 = fixedAssetMatcherFee.id.toString
          docker.restartNode(node,
                             configWithOrderFeeFixed(
                               matcherFeeAssetId = fixedAssetMatcherFeeBase58,
                               minFee = minMatcherFee
                             ))

          val someWavesPair = AssetPair(fixedAssetMatcherFee, Waves)

          val ts                      = ntpTime.correctedTime()
          val expirationTimestamp     = ts + Order.MaxLiveTime - 10000
          val orderAmount             = 1
          val aliceAssetBalanceBefore = node.assetBalance(alice.address, fixedAssetMatcherFeeBase58).balance
          val bobAssetBalanceBefore   = node.assetBalance(bob.address, fixedAssetMatcherFeeBase58).balance
          val aliceWavesBalanceBefore = node.balanceDetails(alice.address).available
          val bobWavesBalanceBefore   = node.balanceDetails(bob.address).available

          val aliceOrderIdFill = node
            .placeOrder(
              Order
                .buy(
                  sender = alice,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = minMatcherFee,
                  version = 3,
                  matcherFeeAssetId = fixedAssetMatcherFee
                ))
            .message
            .id
          val bobSellOrderId = node
            .placeOrder(
              Order
                .sell(
                  sender = bob,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = minMatcherFee,
                  version = 3,
                  matcherFeeAssetId = fixedAssetMatcherFee
                ))
            .message
            .id

          orderStatus(alice, someWavesPair, aliceOrderIdFill, "Filled")
          orderStatus(bob, someWavesPair, bobSellOrderId, "Filled")

          node.waitOrderInBlockchain(aliceOrderIdFill)

          node.assetBalance(alice.address, fixedAssetMatcherFeeBase58).balance shouldBe aliceAssetBalanceBefore - minMatcherFee + orderAmount
          node.assetBalance(bob.address, fixedAssetMatcherFeeBase58).balance shouldBe bobAssetBalanceBefore - minMatcherFee - orderAmount
          node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - (orderAmount * price / 100000000)
          node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore + (orderAmount * price / 100000000)

        }
      }

      "should reject orders if orders' matcherFeeAsset not equal to specified in config" in {
        val ts                  = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime
        val amount              = 1
        val aliceWavesPair      = AssetPair(aliceAsset, Waves)
        val buy                 = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, Waves)
        val sell                = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, Waves)

        assertBadRequestAndResponse(node.placeOrder(buy), f"Required one of the following: $aliceScriptedAssetBase58 as asset fee, but given WAVES")
        assertBadRequestAndResponse(node.placeOrder(sell), f"Required one of the following: $aliceScriptedAssetBase58 as asset fee, but given WAVES")
      }

      "should reject orders if orders' matcherFee less than specified minFee in config" in {
        val ts                     = ntpTime.correctedTime()
        val expirationTimestamp    = ts + Order.MaxLiveTime
        val amount                 = 1
        val aliceWavesPair         = AssetPair(aliceAsset, Waves)
        val insufficientMatcherFee = minMatcherFee - 1
        val buy = Order.buy(alice,
                            matcherPublicKey,
                            aliceWavesPair,
                            amount,
                            price,
                            ts,
                            expirationTimestamp,
                            insufficientMatcherFee,
                            version = 3,
                            aliceScriptedAsset)
        val sell = Order.sell(bob,
                              matcherPublicKey,
                              aliceWavesPair,
                              amount,
                              price,
                              ts,
                              expirationTimestamp,
                              insufficientMatcherFee,
                              version = 3,
                              aliceScriptedAsset)

        assertBadRequestAndMessage(node.placeOrder(buy),
                                   f"Required $minMatcherFee $aliceScriptedAssetBase58 as fee for this order, but given $insufficientMatcherFee")
        assertBadRequestAndMessage(node.placeOrder(sell),
                                   f"Required $minMatcherFee $aliceScriptedAssetBase58 as fee for this order, but given $insufficientMatcherFee")

      }
    }

    "when has percent-mode for fee in config and orders placed" - {
      "should accept orders with amount/price/spending/receiving assets as matcherFeeAsset" in {
        val minFeePercent = 10
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
          for (amountAsset <- Seq(
                 aliceAsset,
                 aliceScriptedAsset
               )) {
            val amountAssetBase58   = amountAsset.id.toString
            val someWavesPair       = AssetPair(amountAsset, Waves)
            val ts                  = ntpTime.correctedTime()
            val expirationTimestamp = ts + Order.MaxLiveTime - 10000
            val amount              = 100L
            val priceAssetSpending  = 0.000001.waves

            val (buyFeeAssetId, sellFeeAssetId, buyMatcherFee, sellMatcherFee) = percentAssetType match {
              case "amount" =>
                (amountAsset, amountAsset, amount * minFeePercent / 100, amount * minFeePercent / 100)
              case "price" =>
                (Waves, Waves, priceAssetSpending * minFeePercent / 100, priceAssetSpending * minFeePercent / 100)
              case "spending" =>
                (Waves, amountAsset, priceAssetSpending * minFeePercent / 100, amount * minFeePercent / 100)
              case "receiving" =>
                (amountAsset, Waves, amount * minFeePercent / 100, priceAssetSpending * minFeePercent / 100)
            }

            val buy =
              Order.buy(alice, matcherPublicKey, someWavesPair, amount, price, ts, expirationTimestamp, buyMatcherFee, version = 3, buyFeeAssetId)
            val sell =
              Order.sell(bob, matcherPublicKey, someWavesPair, amount, price, ts, expirationTimestamp, sellMatcherFee, version = 3, sellFeeAssetId)

            val aliceWavesBalanceBefore = node.balanceDetails(alice.address).available
            val bobWavesBalanceBefore   = node.balanceDetails(bob.address).available
            val aliceAssetBalanceBefore = node.assetBalance(alice.address, amountAssetBase58).balance
            val bobAssetBalanceBefore   = node.assetBalance(bob.address, amountAssetBase58).balance

            val aliceOrderId = node
              .placeOrder(buy)
              .message
              .id
            val bobOrderId = node
              .placeOrder(sell)
              .message
              .id

            orderStatus(alice, someWavesPair, aliceOrderId, "Filled")
            orderStatus(bob, someWavesPair, bobOrderId, "Filled")

            node.waitOrderInBlockchain(aliceOrderId)

            percentAssetType match {
              case "amount" =>
                node.assetBalance(alice.address, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore - buyMatcherFee + amount
                node.assetBalance(bob.address, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - sellMatcherFee - amount
                node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - priceAssetSpending
                node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore + priceAssetSpending
              case "price" =>
                node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - buyMatcherFee - priceAssetSpending
                node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore - sellMatcherFee + priceAssetSpending
                node.assetBalance(alice.address, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore + amount
                node.assetBalance(bob.address, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - amount
              case "spending" =>
                node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - buyMatcherFee - priceAssetSpending
                node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore + priceAssetSpending
                node.assetBalance(alice.address, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore + amount
                node.assetBalance(bob.address, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - sellMatcherFee - amount
              case "receiving" =>
                node.balanceDetails(alice.address).available shouldBe aliceWavesBalanceBefore - priceAssetSpending
                node.balanceDetails(bob.address).available shouldBe bobWavesBalanceBefore - sellMatcherFee + amount
                node.assetBalance(alice.address, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore - buyMatcherFee + amount
                node.assetBalance(bob.address, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - amount

            }
          }
        }
      }

      "should reject orders if matcherFeeAsset not equal to amount/price/spending/receiving assets" in {
        val minFeePercent = 10
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
          val ts                  = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount              = 100
          val aliceWavesPair      = AssetPair(aliceAsset, Waves)

          val (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset) = percentAssetType match {
            case "amount" =>
              val buy = Order.buy(alice,
                                  matcherPublicKey,
                                  aliceWavesPair,
                                  amount,
                                  price,
                                  ts,
                                  expirationTimestamp,
                                  amount * (minFeePercent / 100),
                                  version = 3,
                                  Waves)
              val sell = Order.sell(bob,
                                    matcherPublicKey,
                                    aliceWavesPair,
                                    amount,
                                    price,
                                    ts,
                                    expirationTimestamp,
                                    amount * (minFeePercent / 100),
                                    version = 3,
                                    Waves)
              val requiredBuyMatcherFeeAsset  = aliceAssetBase58
              val requiredSellMatcherFeeAsset = aliceAssetBase58
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "price" =>
              val buy =
                Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val sell =
                Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val requiredBuyMatcherFeeAsset  = "WAVES"
              val requiredSellMatcherFeeAsset = "WAVES"
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "spending" =>
              val buy =
                Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val sell                        = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, Waves)
              val requiredBuyMatcherFeeAsset  = "WAVES"
              val requiredSellMatcherFeeAsset = aliceAssetBase58
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "receiving" =>
              val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, Waves)
              val sell =
                Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, matcherFee, version = 3, aliceAsset)
              val requiredBuyMatcherFeeAsset  = aliceAssetBase58
              val requiredSellMatcherFeeAsset = "WAVES"
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
          }

          assertBadRequestAndResponse(node
                                        .placeOrder(buy),
                                      f"Required one of the following: $requiredBuyMatcherFeeAsset as asset fee")
          assertBadRequestAndResponse(node
                                        .placeOrder(sell),
                                      f"Required one of the following: $requiredSellMatcherFeeAsset as asset fee")

        }
      }

      "should reject orders if orders' matcherFee amount less then specified in config" in {
        val minFeePercent = 10
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
          val amountAssetBase58   = aliceAsset.id.toString
          val someWavesPair       = AssetPair(aliceAsset, Waves)
          val ts                  = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount              = 100L
          val priceAssetSpending  = 0.000001.waves

          val (buyFeeAssetId, sellFeeAssetId, insufficientBuyMatcherFee, insufficientSellMatcherFee) = percentAssetType match {
            case "amount" =>
              (aliceAsset, aliceAsset, (amount * minFeePercent / 100) - 1, (amount * minFeePercent / 100) - 1)
            case "price" =>
              (Waves, Waves, (priceAssetSpending * minFeePercent / 100) - 1, (priceAssetSpending * minFeePercent / 100) - 1)
            case "spending" =>
              (Waves, aliceAsset, (priceAssetSpending * minFeePercent / 100) - 1, (amount * minFeePercent / 100) - 1)
            case "receiving" =>
              (aliceAsset, Waves, (amount * minFeePercent / 100) - 1, (priceAssetSpending * minFeePercent / 100) - 1)
          }

          assertBadRequestAndMessage(
            node.placeOrder(
              Order.buy(alice,
                        matcherPublicKey,
                        someWavesPair,
                        amount,
                        price,
                        ts,
                        expirationTimestamp,
                        insufficientBuyMatcherFee,
                        version = 3,
                        buyFeeAssetId)),
            f"Required ${insufficientBuyMatcherFee + 1} ${buyFeeAssetId.maybeBase58Repr.getOrElse("WAVES")} as fee for this order, but given $insufficientBuyMatcherFee"
          )
          assertBadRequestAndMessage(
            node.placeOrder(
              Order.sell(bob,
                         matcherPublicKey,
                         someWavesPair,
                         amount,
                         price,
                         ts,
                         expirationTimestamp,
                         insufficientSellMatcherFee,
                         version = 3,
                         sellFeeAssetId)),
            f"Required ${insufficientSellMatcherFee + 1} ${sellFeeAssetId.maybeBase58Repr.getOrElse("WAVES")} as fee for this order, but given $insufficientSellMatcherFee"
          )

        }
      }
    }
    "when has insufficient funds in WAVES to pay ExchangeTx fee" in {
      docker.restartNode(node, configWithOrderFeeFixed(matcherFeeAssetId = aliceAssetBase58, matcherFee))
      val matcherWavesBalance = node.balanceDetails(matcher.address).available
      node.broadcastTransfer(matcher, alice.address, matcherWavesBalance - minFee, assetId = None, fee = minFee, feeAssetId = None, waitForTx = true)

      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime - 10000
      val amount              = 100
      val aliceWavesPair      = AssetPair(aliceAsset, Waves)

      val aliceOrderIdFill = node
        .placeOrder(
          Order
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
              matcherFeeAssetId = aliceAsset
            ))
        .message
        .id

      node.placeOrder(
        Order
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
            matcherFeeAssetId = aliceAsset
          ))

      nodes.waitForHeightArise()

      val exchangeTxId = node.transactionsByOrder(aliceOrderIdFill).head.id
      SyncMatcherHttpApi.assertNotFoundAndMessage(node.transactionInfo(exchangeTxId), "Transaction is not in blockchain")
    }

  }

  private def orderStatus(sender: KeyPair, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    node.waitOrderStatus(assetPair, orderId, expectedStatus, waitTime = 2.minutes)

}

object MatcherFeeAssetSuite {

  def configWithOrderFeeFixed(matcherFeeAssetId: String = "WAVES", minFee: Long): Config = {
    parseString(s"""
                   |waves.matcher {
                   | allow-order-v3 = yes
                   |  order-fee {
                   |    mode = "fixed"
                   |
                   |    fixed {
                   |      asset = "$matcherFeeAssetId"
                   |      min-fee = "$minFee"
                   |   }
                   |  }
                   |}""".stripMargin)
  }

  def configWithOrderFeeDynamic(minFee: Long): Config = {
    parseString(s"""
                   |waves.matcher {
                   | allow-order-v3 = yes
                   |  order-fee {
                   |    mode = "dynamic"
                   |
                   |    dynamic {
                   |      base-fee = "$minFee"
                   |   }
                   |  }
                   |}""".stripMargin)
  }

  def configWithOrderFeePercent(assetType: String, minFeePercent: Double): Config = {
    parseString(s"""
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
