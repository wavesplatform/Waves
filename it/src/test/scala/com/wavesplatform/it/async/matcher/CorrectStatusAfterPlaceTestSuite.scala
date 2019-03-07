package com.wavesplatform.it.async.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi.NodesAsyncHttpApi
import com.wavesplatform.it.api.AsyncMatcherHttpApi._
import com.wavesplatform.it.async.matcher.CorrectStatusAfterPlaceTestSuite._
import com.wavesplatform.it.sync.createSignedIssueRequest
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class CorrectStatusAfterPlaceTestSuite extends FreeSpec with Matchers with NodesFromDocker {

  private val matcherConfig = ConfigFactory.parseString(
    s"""waves {
       |  miner.enable = no
       |  matcher {
       |    enable = yes
       |    account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
       |    bind-address = "0.0.0.0"
       |    price-assets = ["${Asset1.id()}", "${Asset2.id()}"]
       |    rest-order-limit = 100
       |    events-queue {
       |      local {
       |        polling-interval = 1s
       |        max-elements-per-poll = 100
       |      }
       |
       |      kafka.consumer {
       |        buffer-size = 100
       |      }
       |    }
       |  }
       |}
       |
       |akka.kafka.consumer {
       |  poll-interval = 1s
       |}""".stripMargin
  )

  private val pairs = Seq(
    AssetPair(Waves, IssuedAsset(Asset1.id())),
    AssetPair(Waves, IssuedAsset(Asset2.id())),
    AssetPair(IssuedAsset(Asset2.id()), IssuedAsset(Asset1.id())),
  )

  override protected val nodeConfigs: Seq[Config] =
    List(NodeConfigs.Default.last, Random.shuffle(NodeConfigs.Default.init).head)
      .zip(List(matcherConfig, ConfigFactory.empty()))
      .map { case (n, o) => o.withFallback(ConfigFactory.parseString("waves.miner.quorum=1")).withFallback(n) }

  private def matcherNode = nodes.head
  private def minerNode   = nodes.last

  private val traders = AllPrivateKeys.filterNot(_ == Issuer).take(10).distinct

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    val startTs    = System.currentTimeMillis()
    val sendAmount = Long.MaxValue / (traders.size + 1)
    val issueAndDistribute = for {
      // issue
      issueTxs <- Future.traverse(Assets)(asset => nodes.head.signedIssue(createSignedIssueRequest(asset)))
      _        <- Future.traverse(issueTxs)(tx => nodes.waitForTransaction(tx.id))

      // distribute
      transferTxs <- Future.sequence {
        Assets.map { issueTx =>
          val transferTx = MassTransferTransaction
            .selfSigned(
              sender = Issuer,
              assetId = IssuedAsset(issueTx.id()),
              transfers = traders.map(x => MassTransferTransaction.ParsedTransfer(x.toAddress, sendAmount)).toList,
              timestamp = startTs,
              feeAmount = 0.006.waves,
              attachment = Array.emptyByteArray
            )
            .explicitGet()

          minerNode.broadcastRequest(transferTx.json())
        }
      }
      _ <- Future.traverse(transferTxs)(tx => nodes.waitForTransaction(tx.id))
    } yield ()

    Await.result(issueAndDistribute, 5.minute)
  }

  "place orders and check their statuses" in {
    val ts = System.currentTimeMillis()

    val orders = for {
      account <- traders
      pair    <- pairs
      i       <- 1 to 60
    } yield matcherNode.prepareOrder(account, pair, OrderType.SELL, 100000L, 10000L, 0.003.waves, 1, timestamp = ts + i)

    val r = Await.result(Future.traverse(orders.grouped(orders.size / 5))(requests), 5.minutes).flatten
    r.foreach {
      case (id, status) => withClue(id)(status should not be "NotFound")
    }
  }

  private def request(order: Order): Future[(String, String)] =
    for {
      _      <- matcherNode.placeOrder(order)
      status <- matcherNode.orderStatus(order.idStr(), order.assetPair, waitForStatus = false)
    } yield (order.idStr(), status.status)

  private def requests(orders: Seq[Order]): Future[Seq[(String, String)]] = Future.traverse(orders)(request)
}

object CorrectStatusAfterPlaceTestSuite {
  private val AllPrivateKeys = NodeConfigs.Default.map(c => PrivateKeyAccount.fromSeed(c.getString("account-seed")).right.get)
  private val Issuer         = AllPrivateKeys.head

  private val Asset1 = IssueTransactionV1
    .selfSigned(
      sender = Issuer,
      name = "asset1".getBytes,
      description = Array.emptyByteArray,
      quantity = Long.MaxValue,
      decimals = 0,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  private val Asset2 = IssueTransactionV1
    .selfSigned(
      sender = Issuer,
      name = "asset2".getBytes,
      description = Array.emptyByteArray,
      quantity = Long.MaxValue,
      decimals = 0,
      reissuable = false,
      fee = 1.waves,
      timestamp = System.currentTimeMillis()
    )
    .explicitGet()

  private val Assets = List(Asset1, Asset2)
}
