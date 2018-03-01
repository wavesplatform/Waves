package com.wavesplatform.matcher.model

import cats.implicits._
import com.wavesplatform.UtxPool
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import com.wavesplatform.matcher.model.Events.OrderAdded
import scorex.account.PublicKeyAccount
import scorex.transaction.AssetAcc
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderValidator {
  val orderHistory: OrderHistory
  val utxPool: UtxPool
  val settings: MatcherSettings
  val wallet: Wallet

  lazy val matcherPubKey: PublicKeyAccount = wallet.findWallet(settings.account).right.get
  val MinExpiration = 60 * 1000L

  def isBalanceWithOpenOrdersEnough(order: Order): Validation = {
    val lo = LimitOrder(order)

    val b: Map[String, Long] = (Map(lo.spentAcc -> 0L) ++ Map(lo.feeAcc -> 0L))
      .map { case(a, _) => a -> spendableBalance(a) }
      .map { case(a, v) => a.assetId.map(_.base58).getOrElse(AssetPair.WavesName) -> v }

    val newOrder = Events.createOpenPortfolio(OrderAdded(lo)).getOrElse(order.senderPublicKey.address, OpenPortfolio.empty)
    val open = orderHistory.openPortfolio(order.senderPublicKey.address).orders.filter { case (k, _) => b.contains(k) }
    val needs = OpenPortfolio(open).combine(newOrder)

    val res: Boolean = b.combine(needs.orders.mapValues(-_)).forall(_._2 >= 0)

    res :| s"Not enough tradable balance: ${b.combine(open.mapValues(-_))}, needs: $newOrder"
  }

  def getTradableBalance(acc: AssetAcc): Long = {
    math.max(0l, spendableBalance(acc) - orderHistory.openVolume(acc))
  }

  def validateNewOrder(order: Order): Either[GenericError, Order] = {
    val v =
    (order.matcherPublicKey == matcherPubKey) :| "Incorrect matcher public key" &&
      (order.expiration > NTP.correctedTime() + MinExpiration) :| "Order expiration should be > 1 min" &&
      order.signaturesValid().isRight :| "signature should be valid" &&
      order.isValid(NTP.correctedTime()) &&
      (order.matcherFee >= settings.minOrderFee) :| s"Order matcherFee should be >= ${settings.minOrderFee}" &&
      (orderHistory.orderStatus(order.idStr()) == LimitOrder.NotFound) :| "Order is already accepted" &&
      isBalanceWithOpenOrdersEnough(order)
    if (!v) {
      Left(GenericError(v.messages()))
    } else {
      Right(order)
    }
  }

  def validateCancelOrder(cancel: CancelOrder): Either[GenericError, CancelOrder] = {
    val status = orderHistory.orderStatus(cancel.orderId)
    val v =
      (status != LimitOrder.NotFound) :| "Order not found" &&
        (status != LimitOrder.Filled) :| "Order is already Filled" &&
        cancel.req.isSignatureValid :| "Signature should be valid" &&
        orderHistory.order(cancel.orderId).fold(false)(_.senderPublicKey == cancel.req.senderPublicKey)  :| "Order not found"

    if (!v) {
      Left(GenericError(v.messages()))
    } else {
      Right(cancel)
    }
  }

  private def spendableBalance(a: AssetAcc): Long = {
    val portfolio = utxPool.portfolio(a.account)
    a.assetId match {
      case Some(x) => portfolio.assets.getOrElse(x, 0)
      case None => portfolio.spendableBalance
    }
  }
}
