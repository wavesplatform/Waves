package com.wavesplatform.matcher.market

import java.util

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.market.MatcherTransactionWriter.{GetTransactionsByOrder, GetTransactionsResponse}
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.utils
import org.h2.mvstore.MVStore
import play.api.libs.json.JsArray
import scorex.transaction.assets.exchange.ExchangeTransaction

class MatcherTransactionWriter(val settings: MatcherSettings)
  extends Actor {

  val db: MVStore = utils.createStore(settings.txHistoryFile)
  val transactions: util.Map[String, Array[Byte]] = db.openMap("transactions")
  val orderToTxIds: util.Map[String, Set[String]] = db.openMap("orderToTxIds")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
  }

  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) =>
      saveExchangeTx(tx)
    case GetTransactionsByOrder(orderId) =>
      fetchTransationsByOrder(orderId)
  }

  def fetchTransationsByOrder(orderId: String): Unit = {
    val txs = Option(orderToTxIds.get(orderId)).getOrElse(Set()).flatMap(id => Option(transactions.get(id)))
      .flatMap(b => ExchangeTransaction.parseBytes(b).toOption)

    sender() ! GetTransactionsResponse(txs.toSeq)
  }

  private def saveOrder2TxId(orderId: String, txId: String) = {
    Option(orderToTxIds.get(orderId)) match {
      case Some(prev) =>
        orderToTxIds.put(orderId, prev + txId)
      case _ => orderToTxIds.put(orderId, Set(txId))
    }
  }

  private def saveExchangeTx(tx: ExchangeTransaction) = {
    val txId = tx.id().toString
    transactions.put(txId, tx.bytes())
    saveOrder2TxId(tx.buyOrder.idStr(), txId)
    saveOrder2TxId(tx.sellOrder.idStr(), txId)
  }
}

object MatcherTransactionWriter {
  def name = "MatcherTransactionWriter"
  def props(settings: MatcherSettings): Props = Props(new MatcherTransactionWriter(settings))

  case class GetTransactionsByOrder(orderId: String)
  case class GetTransactionsResponse(txs: Seq[ExchangeTransaction]) extends MatcherResponse {
    val json = JsArray(txs.map(_.json()))
    val code = StatusCodes.OK
  }
}
