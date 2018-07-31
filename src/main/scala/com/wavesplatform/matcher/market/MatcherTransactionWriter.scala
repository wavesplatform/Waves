package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.db.{OrderToTxIdsCodec, SubStorage}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.state._
import org.iq80.leveldb.DB
import play.api.libs.json.JsArray
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

class MatcherTransactionWriter(db: DB, val settings: MatcherSettings) extends SubStorage(db, "matcher") with Actor {

  import MatcherTransactionWriter._

  private val TransactionsPrefix  = "transactions".getBytes(Charset)
  private val OrdersToTxIdsPrefix = "ord-to-tx-ids".getBytes(Charset)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
  }

  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) =>
      saveExchangeTx(tx)
    case GetTransactionsByOrder(orderId) =>
      fetchTransactionsByOrder(orderId)
  }

  def fetchTransactionsByOrder(orderId: String): Unit = {
    val txs = get(makeKey(OrdersToTxIdsPrefix, orderId))
      .map(OrderToTxIdsCodec.decode)
      .map(_.explicitGet().value)
      .getOrElse(Set())
      .flatMap(id => get(makeKey(TransactionsPrefix, id)))
      .flatMap(b => ExchangeTransaction.parseBytes(b).toOption)

    sender() ! GetTransactionsResponse(txs.toSeq)
  }

  private def saveOrder2TxId(orderId: String, txId: String): Unit = {
    val key = makeKey(OrdersToTxIdsPrefix, orderId)
    get(key) match {
      case Some(bytes) =>
        val prev = OrderToTxIdsCodec.decode(bytes).explicitGet().value
        put(key, OrderToTxIdsCodec.encode(prev + txId), None)
      case _ => put(key, OrderToTxIdsCodec.encode(Set(txId)), None)
    }
  }

  private def saveExchangeTx(tx: ExchangeTransaction): Unit = {
    val txId = tx.id().base58
    put(makeKey(TransactionsPrefix, txId), tx.bytes(), None)
    saveOrder2TxId(tx.buyOrder.idStr(), txId)
    saveOrder2TxId(tx.sellOrder.idStr(), txId)
  }
}

object MatcherTransactionWriter {

  def name: String = "MatcherTransactionWriter"

  def props(db: DB, settings: MatcherSettings): Props = Props(new MatcherTransactionWriter(db, settings))

  case class GetTransactionsByOrder(orderId: String)

  case class GetTransactionsResponse(txs: Seq[ExchangeTransaction]) extends MatcherResponse {
    val json                      = JsArray(txs.map(_.json()))
    val code: StatusCodes.Success = StatusCodes.OK
  }

}
