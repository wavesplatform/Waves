package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import scorex.account.PublicKeyAccount
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderValidator {
  this: OrderHistory =>
  val storedState: StoredState
  val settings: MatcherSettings
  val wallet: Wallet

  lazy val matcherPubKey: PublicKeyAccount = wallet.privateKeyAccount(settings.account).get

  def isBalanceWithOpenOrdersEnough(order: Order): Boolean = {
    val (acc, feeAcc) = (AssetAcc(order.senderPublicKey, order.getSpendAssetId), AssetAcc(order.senderPublicKey, None))

    val (assBal, feeBal) = (storedState.assetBalance(acc) - assetsToSpend.getOrElse(acc.key, 0L),
      storedState.assetBalance(feeAcc) - assetsToSpend.getOrElse(feeAcc.key, 0L))

    if (acc != feeAcc) assBal >= order.getSpendAmount(order.price, order.amount).get && feeBal >= order.matcherFee
    else assBal >= order.getSpendAmount(order.price, order.amount).get + order.matcherFee
  }

  def validateIntegerAmount(lo: LimitOrder): Validation = {
    val asset = if (lo.price >= Order.PriceConstant) lo.order.assetPair.priceAsset else lo.order.assetPair.amountAsset
    val amount =  if (lo.price >= Order.PriceConstant)
      (BigInt(lo.amount) * lo.price / Order.PriceConstant).bigInteger.longValueExact()
      else lo.amount
    val decimals = asset.flatMap(storedState.getIssueTransaction).map(_.decimals.toInt).getOrElse(8)
    val scaled = BigDecimal(amount, decimals)
    (scaled >= 1) :| "Order amount is too small"
  }

  def validateNewOrder(order: Order): Validation = {
    (openOrdersCount.getOrElse(order.matcherPublicKey.address, 0) <= settings.maxOpenOrders) :|
      s"Open orders count limit exceeded (Max = ${settings.maxOpenOrders})" &&
      (order.matcherPublicKey == matcherPubKey) :| "Incorrect matcher public key" &&
      validateIntegerAmount(LimitOrder(order)) &&
      order.isValid(NTP.correctedTime()) &&
      (order.matcherFee >= settings.minOrderFee) :| s"Order matcherFee should be >= ${settings.minOrderFee}" &&
      !ordersRemainingAmount.contains(order.idStr) :| "Order is already accepted" &&
      isBalanceWithOpenOrdersEnough(order) :| "Not enough balance"
  }

  def validateCancelOrder(cancel: CancelOrder): Validation = {
    ordersRemainingAmount.contains(cancel.orderId) :| "Order not found" &&
      (getOrderStatus(cancel.orderId) != LimitOrder.Filled) :| "Order is already Filled" &&
      cancel.req.isSignatureValid :| "Signature should be valid"

  }
}
