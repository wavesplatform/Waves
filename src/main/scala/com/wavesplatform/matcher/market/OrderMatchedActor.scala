package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import com.wavesplatform.matcher.market.OrderBookActor.OrderMatched
import com.wavesplatform.matcher.model.OrderItem
import com.wavesplatform.settings.WavesSettings
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet
import scorex.waves.transaction.WavesTransactionModule

object OrderMatchedActor {
  def props(transactionModule: WavesTransactionModule, settings: WavesSettings,
            storedState: StoredState, wallet: Wallet): Props =
    Props(new OrderMatchedActor(transactionModule, settings, storedState, wallet))
  val name = "OrderMatchedActor"
}

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
      val (buy, sell) = Order.splitByType(order, item.order)
      val (buyFee, sellFee) =  calculateMatcherFee(buy, sell, amount: Long)
      transactionModule.createOrderMatch(buy, sell, price, amount, buyFee, sellFee, settings.matcherTxFee, wallet)
    }
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      storedState.findPrevOrderMatchTxs(o)
      val p = BigInt(amount) * o.matcherFee  / o.amount
      p.toLong
    }
    (calcFee(buy, amount), calcFee(sell, amount))
  }

}
