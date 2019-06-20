package com.wavesplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.ReportingTestName
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.sync.matcher.BlockedAssetsTestSuite._
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransactionV2, Order, OrderV2}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV2}
import org.scalatest._

import scala.concurrent.duration.DurationInt
class BlockedAssetsTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {
  override protected def nodeConfigs: Seq[Config] = Configs.map(ConfigOverrides.withFallback)

  private def matcher = dockerNodes().head
  private def alice   = dockerNodes()(1)
  private def miner   = dockerNodes()(2)

  val (amount, price) = (1000L, Order.PriceConstant)

  private val asset = {
    val txId   = miner.signedBroadcast(IssueUsdTx.json()).id
    val height = miner.waitForTransaction(txId).height
    nodes.waitForHeight(height + 1)
    IssueUsdTx
  }

  private def order      = matcher.prepareOrder(matcher.privateKey, wavesUsdPair, BUY, amount, price, matcherFee)
  private val toTransfer = LimitOrder(order).requiredBalance(Some(asset.id())) * 2
  private def reverseTransferTx =
    TransferTransactionV2
      .selfSigned(
        assetId = Some(asset.id()),
        sender = matcher.privateKey,
        recipient = miner.publicKey.toAddress,
        amount = toTransfer,
        timestamp = System.currentTimeMillis(),
        feeAssetId = None,
        feeAmount = 300000L,
        attachment = Array.emptyByteArray
      )
      .explicitGet()

  "Do not track from allowed sender" - {
    "TransferTransaction" in {
      val transferTx = TransferTransactionV2
        .selfSigned(
          assetId = Some(asset.id()),
          sender = alice.privateKey,
          recipient = miner.publicKey.toAddress,
          amount = toTransfer,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = 300000L,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
      miner.signedBroadcast(transferTx.json(), waitForTx = true)
      nodes.waitForTransaction(transferTx.id().base58)

      withClue("after transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("alice") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(alice.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(alice.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity - toTransfer
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer
          }
        }
      }
    }

    "MassTransferTransaction" in {
      val transferTx = MassTransferTransaction
        .selfSigned(
          assetId = Some(asset.id()),
          sender = alice.privateKey,
          transfers = List(MassTransferTransaction.ParsedTransfer(miner.publicKey.toAddress, toTransfer)),
          timestamp = System.currentTimeMillis(),
          feeAmount = 300000L,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
      miner.signedBroadcast(transferTx.json(), waitForTx = true)
      nodes.waitForTransaction(transferTx.id().base58)

      withClue("after transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("alice") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(alice.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(alice.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity - toTransfer * 2
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer * 2
          }
        }
      }
    }

    "ExchangeTransaction" in {
      val now   = System.currentTimeMillis()
      val price = Order.PriceConstant
      val exchangeTx = ExchangeTransactionV2
        .create(
          matcher = alice.privateKey,
          buyOrder = OrderV2.buy(
            sender = alice.privateKey,
            matcher = alice.publicKey,
            pair = wavesUsdPair,
            amount = asset.quantity - toTransfer * 2,
            price = price,
            timestamp = now,
            expiration = now + 10.days.toMillis,
            matcherFee = 3000000
          ),
          sellOrder = OrderV2.sell(
            sender = miner.privateKey,
            matcher = alice.publicKey,
            pair = wavesUsdPair,
            amount = asset.quantity - toTransfer * 2,
            price = price,
            timestamp = now,
            expiration = now + 10.days.toMillis,
            matcherFee = 3000000
          ),
          amount = asset.quantity - toTransfer * 2,
          price = price,
          buyMatcherFee = 3000000,
          sellMatcherFee = 3000000,
          fee = 3000000,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()
      miner.signedBroadcast(exchangeTx.json(), waitForTx = true)
      nodes.waitForTransaction(exchangeTx.id().base58)

      withClue("after transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("alice") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(alice.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(alice.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity
          }
        }
      }
    }
  }

  "Track" - {
    "TransferTransaction" in {
      val transferTx = TransferTransactionV2
        .selfSigned(
          assetId = Some(asset.id()),
          sender = miner.privateKey,
          recipient = matcher.publicKey.toAddress,
          amount = toTransfer,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = 300000L,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
      miner.signedBroadcast(transferTx.json(), waitForTx = true)
      nodes.waitForTransaction(transferTx.id().base58)

      withClue("after transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity - toTransfer
          }
        }
      }

      matcher.expectIncorrectOrderPlacement(order, 400, "OrderRejected")

      val reverseTransferTxId = miner.signedBroadcast(reverseTransferTx.json(), waitForTx = true).id
      nodes.waitForTransaction(reverseTransferTxId)

      withClue("after reverse transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity
          }
        }
      }
    }

    "MassTransferTransaction" in {
      val transferTx = MassTransferTransaction
        .selfSigned(
          assetId = Some(asset.id()),
          sender = miner.privateKey,
          transfers = List(MassTransferTransaction.ParsedTransfer(matcher.publicKey.toAddress, toTransfer)),
          timestamp = System.currentTimeMillis(),
          feeAmount = 300000L,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
      miner.signedBroadcast(transferTx.json(), waitForTx = true)
      nodes.waitForTransaction(transferTx.id().base58)

      withClue("after transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity - toTransfer
          }
        }
      }

      matcher.expectIncorrectOrderPlacement(order, 400, "OrderRejected")

      val reverseTransferTxId = matcher.signedBroadcast(reverseTransferTx.json(), waitForTx = true).id
      nodes.waitForTransaction(reverseTransferTxId)

      withClue("after reverse transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity
          }
        }
      }
    }

    "ExchangeTransaction from different matcher" in {
      val now   = System.currentTimeMillis()
      val price = Order.PriceConstant
      val exchangeTx = ExchangeTransactionV2
        .create(
          matcher = miner.privateKey,
          buyOrder = OrderV2.buy(
            sender = miner.privateKey,
            matcher = miner.publicKey,
            pair = wavesUsdPair,
            amount = toTransfer,
            price = price,
            timestamp = now,
            expiration = now + 10.days.toMillis,
            matcherFee = 3000000
          ),
          sellOrder = OrderV2.sell(
            sender = matcher.privateKey,
            matcher = miner.publicKey,
            pair = wavesUsdPair,
            amount = toTransfer,
            price = price,
            timestamp = now,
            expiration = now + 10.days.toMillis,
            matcherFee = 3000000
          ),
          amount = toTransfer,
          price = price,
          buyMatcherFee = 3000000,
          sellMatcherFee = 3000000,
          fee = 3000000,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()
      miner.signedBroadcast(exchangeTx.json(), waitForTx = true)
      nodes.waitForTransaction(exchangeTx.id().base58)

      withClue("after exchange") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe toTransfer
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity - toTransfer
          }
        }
      }

      matcher.expectIncorrectOrderPlacement(order, 400, "OrderRejected")

      val reverseTransferTxId = matcher.signedBroadcast(reverseTransferTx.json(), waitForTx = true).id
      nodes.waitForTransaction(reverseTransferTxId)

      withClue("after reverse transfer") {
        withClue("matcher") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(matcher.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(matcher.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }
        }

        withClue("miner") {
          withClue("reserved") {
            val reservedBalance = matcher.reservedBalance(miner.privateKey)
            reservedBalance.getOrElse(asset.id().base58, 0) shouldBe 0
          }

          withClue("tradable") {
            val tradableBalance = matcher.tradableBalance(miner.privateKey, wavesUsdPair)
            tradableBalance.getOrElse(asset.id().base58, 0) shouldBe asset.quantity
          }
        }
      }
    }
  }
}

object BlockedAssetsTestSuite {
  private def ConfigOverrides = ConfigFactory.parseString(s"""waves.blockchain.custom.functionality.tracked-assets {
                                                             |  allowed-sources = ["${Configs(1).getString("address")}"]
                                                             |  assets = ["${IssueUsdTx.id().base58}"]
                                                             |}""".stripMargin)
}
