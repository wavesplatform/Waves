package com.wavesplatform.matcher.market

import akka.actor.Actor
import com.wavesplatform.matcher.market.OrderBookActor.OrderMatched
import com.wavesplatform.matcher.model.OrderItem
import com.wavesplatform.settings.WavesSettings
import scorex.transaction.assets.exchange.Order
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet
import scorex.waves.transaction.WavesTransactionModule

class OrderMatchedActor(transactionModule: WavesTransactionModule,
                        settings: WavesSettings,
                        storedState: StoredState,
                        wallet: Wallet) extends Actor {

  override def receive: Receive = {
    case OrderMatched(order, items) =>
      createTransactions(order, items)
  }

  def createTransactions(order: Order, items: Seq[OrderItem]): Unit = {
    items.foreach { item =>
      val price = order.price
      val amount = item.amount
      val (buy, sell) = Order.sortByType(order, item.order)
      val (buyFee, sellFee) =  calculateMatcherFee(buy, sell, amount: Long)
      transactionModule.createOrderMatch(order, item.order, price, amount, buyFee, sellFee, settings.matcherTxFee, wallet)
    }
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      storedState.findPrevOrderMatchTxs(o)
      val p = BigInt(o.amount) * o.matcherFee  / amount
      p.toLong
    }
    (calcFee(buy, amount), calcFee(sell, amount))
  }

}
