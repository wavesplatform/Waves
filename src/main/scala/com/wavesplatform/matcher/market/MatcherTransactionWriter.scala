package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.database.RW
import com.wavesplatform.matcher.{MatcherKeys, MatcherSettings}
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.state._
import org.iq80.leveldb.DB
import play.api.libs.json.JsArray
import scorex.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.database.DBExt

class MatcherTransactionWriter(db: DB) extends Actor {

  import MatcherTransactionWriter._

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
  }

  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) =>
      saveExchangeTx(tx)
    case GetTransactionsByOrder(orderId) =>
      fetchTransactionsByOrder(orderId)
  }

  def fetchTransactionsByOrder(orderId: ByteStr): Unit = {
    val txs: Seq[ExchangeTransaction] = db.readOnly { ro =>
      for {
        seqNr <- 1 to ro.get(MatcherKeys.orderTxIdsSeqNr(orderId))
        txId = ro.get(MatcherKeys.orderTxId(orderId, seqNr))
        tx <- ro.get(MatcherKeys.exchangeTransaction(txId))
      } yield tx
    }

    sender() ! GetTransactionsResponse(txs)
  }

  private def saveExchangeTx(tx: ExchangeTransaction): Unit = db.readWrite { rw =>
    appendTxId(rw, tx.buyOrder.id(), tx.id())
    appendTxId(rw, tx.sellOrder.id(), tx.id())
  }
}

object MatcherTransactionWriter {

  def name: String = "MatcherTransactionWriter"

  def props(db: DB, settings: MatcherSettings): Props = Props(new MatcherTransactionWriter(db))

  case class GetTransactionsByOrder(orderId: ByteStr)

  case class GetTransactionsResponse(txs: Seq[ExchangeTransaction]) extends MatcherResponse {
    val json                      = JsArray(txs.map(_.json()))
    val code: StatusCodes.Success = StatusCodes.OK
  }

  private def appendTxId(rw: RW, orderId: ByteStr, txId: ByteStr): Unit = {
    val key       = MatcherKeys.orderTxIdsSeqNr(orderId)
    val nextSeqNr = rw.get(key) + 1
    rw.put(key, nextSeqNr)
    rw.put(MatcherKeys.orderTxId(orderId, nextSeqNr), txId)
  }
}
