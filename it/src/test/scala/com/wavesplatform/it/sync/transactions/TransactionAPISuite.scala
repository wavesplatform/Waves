package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.{Node, NodeConfigs, ReportingTestName}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionV1}
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.JsNumber

import scala.concurrent.duration._

class TransactionAPISuite extends FreeSpec with NodesFromDocker with Matchers with ReportingTestName with CancelAfterFailure {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.rest-api.transactions-by-address-limit=10"))
      .withDefault(1)
      .withSpecial(1, _.nonMiner)
      .buildNonConflicting()

  val sender: Node       = nodes.head
  val recipient: Address = Address.fromString(sender.createAddress()).explicitGet()

  val Waves: Long = 100000000L

  val AMT: Long = 1 * Waves
  val FEE: Long = (0.001 * Waves).toLong

  val transactions: List[TransferTransaction] =
    (for (i <- 0 to 30) yield {
      TransferTransactionV1
        .selfSigned(
          Asset.Waves,
          sender.privateKey,
          recipient,
          AMT,
          System.currentTimeMillis() + i,
          Asset.Waves,
          FEE + i * 100,
          Array.emptyByteArray
        )
        .explicitGet()
    }).toList

  val transactionIds = transactions.map(_.id().base58)

  "should accept transactions" in {
    transactions.foreach { tx =>
      sender.broadcastRequest(tx.json() + ("type" -> JsNumber(tx.builder.typeId.toInt)))
    }

    val h = sender.height

    sender.waitForHeight(h + 3, 2.minutes)
  }

  "should return correct N txs on request without `after`" in {

    def checkForLimit(limit: Int): Unit = {
      val expected =
        transactionIds
          .take(limit)

      val received =
        sender
          .transactionsByAddress(recipient.address, limit)
          .flatten
          .map(_.id)

      expected shouldEqual received
    }

    for (limit <- 2 to 10 by 1) {
      checkForLimit(limit)
    }
  }

  "should return correct N txs on request with `after`" in {

    def checkForLimit(limit: Int): Unit = {
      val expected =
        transactionIds
          .slice(limit, limit + limit)

      val afterParam =
        transactions
          .drop(limit - 1)
          .head
          .id()
          .base58

      val received =
        sender
          .transactionsByAddress(recipient.address, limit, afterParam)
          .flatten
          .map(_.id)

      expected shouldEqual received
    }

    for (limit <- 2 to 10 by 1) {
      checkForLimit(limit)
    }
  }

  "should return all transactions" in {
    def checkForLimit(limit: Int): Unit = {
      val received =
        loadAll(sender, recipient.address, limit, None, Nil)
          .map(_.id)

      received shouldEqual transactionIds
    }

    for (limit <- 2 to 10 by 1) {
      checkForLimit(limit)
    }
  }

  def loadAll(node: Node, address: String, limit: Int, maybeAfter: Option[String], acc: List[TransactionInfo]): List[TransactionInfo] = {
    val txs = maybeAfter match {
      case None         => node.transactionsByAddress(address, limit).flatten.toList
      case Some(lastId) => node.transactionsByAddress(address, limit, lastId).flatten.toList
    }

    txs.lastOption match {
      case None     => acc ++ txs
      case Some(tx) => loadAll(node, address, limit, Some(tx.id), acc ++ txs)
    }
  }
}
