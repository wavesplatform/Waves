package com.wavesplatform.matcher.model

import com.wavesplatform.settings.WavesSettings
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange.{Order, OrderMatch}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderMatchCreator {
  val transactionModule: TransactionModule[StoredInBlock]
  val storedState: StoredState
  val wallet: Wallet
  val settings: WavesSettings

  private var txTime: Long = 0

  private def getTimestamp: Long = {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createTransaction(order: Order, limitOrder: LimitOrder): OrderMatch = {
    val matcher = wallet.privateKeyAccount(order.matcher.address).get

    val price = order.price
    val amount = limitOrder.amount
    val (buy, sell) = Order.splitByType(order, limitOrder.order)
    val (buyFee, sellFee) =  calculateMatcherFee(buy, sell, amount: Long)
    OrderMatch.create(matcher, buy, sell, price, amount, buyFee, sellFee, settings.matcherTxFee, getTimestamp)
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      storedState.findPrevOrderMatchTxs(o)
      val p = BigInt(amount) * o.matcherFee  / o.amount
      p.toLong
    }
    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def isValid(orderMatch: OrderMatch): Boolean = {
    transactionModule.isValid(orderMatch, orderMatch.timestamp)
  }

  def sendToNetwork(txs: Seq[OrderMatch]): Unit = {
    txs.foreach(sendToNetwork)
  }

  def sendToNetwork(tx: OrderMatch): Unit = {
    transactionModule.onNewOffchainTransaction(tx)
  }
}
