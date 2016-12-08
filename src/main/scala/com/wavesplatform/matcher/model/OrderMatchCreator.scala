package com.wavesplatform.matcher.model

import com.wavesplatform.settings.WavesSettings
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.{SignedTransaction, TransactionModule}
import scorex.transaction.assets.exchange.{Order, OrderCancelTransaction, OrderMatch}
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

  def createTransaction(sumbitted: LimitOrder, counter: LimitOrder): OrderMatch = {
    val matcher = wallet.privateKeyAccount(sumbitted.order.matcher.address).get

    val price = counter.price
    val amount = math.min(sumbitted.amount, counter.amount)
    val (buy, sell) = Order.splitByType(sumbitted.order, counter.order)
    val (buyFee, sellFee) =  calculateMatcherFee(buy, sell, amount: Long)
    OrderMatch.create(matcher, buy, sell, price, amount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
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

  def sendToNetwork(tx: SignedTransaction): Unit = {
    transactionModule.onNewOffchainTransaction(tx)
  }
}
