package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.twitter.chill.{KryoInstantiator, KryoPool}
import com.wavesplatform.db.SubStorage
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.model.Events._
import org.iq80.leveldb.DB
import play.api.libs.json.JsArray
import scorex.transaction.assets.exchange.ExchangeTransaction

class MatcherTransactionWriter(db: DB, val settings: MatcherSettings)
  extends SubStorage(db, "matcher") with Actor {

  import MatcherTransactionWriter._

  private val TransactionsPrefix = "transactions".getBytes(Charset)
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
    val txs = get(makeKey(OrdersToTxIdsPrefix, orderId)).map(decodeOrderToTxIdsValue).getOrElse(Set())
      .flatMap(id => get(makeKey(TransactionsPrefix, id)))
      .flatMap(b => ExchangeTransaction.parseBytes(b).toOption)

    sender() ! GetTransactionsResponse(txs.toSeq)
  }

  private def saveOrder2TxId(orderId: String, txId: String) = {
    val key = makeKey(OrdersToTxIdsPrefix, orderId)
    get(key) match {
      case Some(bytes) =>
        val prev = decodeOrderToTxIdsValue(bytes)
        put(key, encodeOrderToTxIdsValue(prev + txId))
      case _ => put(key, encodeOrderToTxIdsValue(Set(txId)))
    }
  }

  private def saveExchangeTx(tx: ExchangeTransaction) = {
    val txId = tx.id.toString
    put(makeKey(TransactionsPrefix, txId), tx.bytes)
    saveOrder2TxId(tx.buyOrder.idStr, txId)
    saveOrder2TxId(tx.sellOrder.idStr, txId)
  }
}

object MatcherTransactionWriter {
  private val PoolSize = 10
  private val kryo = KryoPool.withByteArrayOutputStream(PoolSize, new KryoInstantiator())

  def encodeOrderToTxIdsValue(value: Set[String]): Array[Byte] = kryo.toBytesWithClass(value)

  def decodeOrderToTxIdsValue(arr: Array[Byte]): Set[String] = kryo.fromBytes(arr, classOf[Set[String]])

  def name: String = "MatcherTransactionWriter"

  def props(db: DB, settings: MatcherSettings): Props = Props(new MatcherTransactionWriter(db, settings))

  case class GetTransactionsByOrder(orderId: String)

  case class GetTransactionsResponse(txs: Seq[ExchangeTransaction]) extends MatcherResponse {
    val json = JsArray(txs.map(_.json()))
    val code = StatusCodes.OK
  }

}
