package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.{MatcherKeys, MatcherSettings}
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB
import play.api.libs.json.JsArray

class MatcherTransactionWriter(db: DB) extends Actor with ScorexLogging {

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
    log.trace(s"Loading transactions for order $orderId")
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
    log.trace(s"Appending ${tx.id()} to orders [${tx.buyOrder.idStr()}, ${tx.sellOrder.idStr()}]")
    rw.put(MatcherKeys.exchangeTransaction(tx.id()), Some(tx))
    appendTxId(rw, ByteStr(tx.buyOrder.id()), tx.id())
    appendTxId(rw, ByteStr(tx.sellOrder.id()), tx.id())
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
