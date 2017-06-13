package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.api.http.AddressApiRoute.Signed
import scorex.transaction.AssetAcc
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderValidator {
  val orderHistory: OrderHistory
  val storedState: StateReader
  val settings: MatcherSettings
  val wallet: Wallet

  lazy val matcherPubKey: PublicKeyAccount = wallet.findWallet(settings.account).right.get

  def isBalanceWithOpenOrdersEnough(order: Order): Validation = {
    def notEnoughError(tBal: Long, oBal: Long, needs: Long): String = s"Not enough balance: [$tBal, $oBal], needs: $needs"

    val (acc, feeAcc) = (AssetAcc(order.senderPublicKey, order.getSpendAssetId), AssetAcc(order.senderPublicKey, None))

    val (assTBal, assOBal) = (storedState.spendableBalance(acc), orderHistory.openVolume(acc))
    val (feeTBal, feeOBal) = (storedState.spendableBalance(feeAcc), orderHistory.openVolume(feeAcc))

    if (acc != feeAcc) {
      (assTBal - assOBal >= order.getSpendAmount(order.price, order.amount).getOrElse(0L)) :|
        notEnoughError(assTBal, assOBal, order.getSpendAmount(order.price, order.amount).getOrElse(0L)) &&
        (feeTBal - feeOBal >= order.matcherFee) :| notEnoughError(feeTBal, feeOBal, order.matcherFee)
    }
    else {
      (assTBal - assOBal >= order.getSpendAmount(order.price, order.amount).getOrElse(0L) + order.matcherFee) :|
        notEnoughError(assTBal, assOBal, order.getSpendAmount(order.price, order.amount).getOrElse(0L) + order.matcherFee)
    }
  }

  def getTradableBalance(acc: AssetAcc): Long = {
    math.max(0l, storedState.spendableBalance(acc) - orderHistory.openVolume(acc))
  }

  def validateNewOrder(order: Order): Either[GenericError, Order] = {
    //(openOrdersCount.getOrElse(order.matcherPublicKey.address, 0) <= settings.maxOpenOrders) :|
    //  s"Open orders count limit exceeded (Max = ${settings.maxOpenOrders})" &&
    val v =
    (order.matcherPublicKey == matcherPubKey) :| "Incorrect matcher public key" &&
      LimitOrder.validateIntegerAmount(storedState, LimitOrder(order)) &&
      scorex.transaction.Signed.validateSignatures(order).isRight :| "signature should be valid"
      order.isValid(NTP.correctedTime()) &&
      (order.matcherFee >= settings.minOrderFee) :| s"Order matcherFee should be >= ${settings.minOrderFee}" &&
      (orderHistory.orderStatus(order.idStr) == LimitOrder.NotFound) :| "Order is already accepted" &&
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
}
