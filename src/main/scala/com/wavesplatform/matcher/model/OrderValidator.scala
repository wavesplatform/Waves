package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.transaction.{AssetAcc, AssetId}
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderValidator {
  this: OrderHistoryOld =>
  val storedState: StateReader
  val settings: MatcherSettings
  val wallet: Wallet

  lazy val matcherPubKey: PublicKeyAccount = wallet.findWallet(settings.account).right.get

  def isBalanceWithOpenOrdersEnough(order: Order): Boolean = {
    val (acc, feeAcc) = (AssetAcc(order.senderPublicKey, order.getSpendAssetId), AssetAcc(order.senderPublicKey, None))

    val (accBal, feeBal) = (storedState.spendableBalance(acc) - assetsToSpend.getOrElse(acc.key, 0L),
      storedState.spendableBalance(feeAcc) - assetsToSpend.getOrElse(feeAcc.key, 0L))

    if (acc != feeAcc) accBal >= order.getSpendAmount(order.price, order.amount).right.get && feeBal >= order.matcherFee
    else accBal >= order.getSpendAmount(order.price, order.amount).right.get + order.matcherFee
  }

  def validateIntegerAmount(lo: LimitOrder): Validation = {
    def getDecimals(assetId: Option[AssetId]) = assetId.flatMap(storedState.getIssueTransaction).map(_.decimals.toInt).getOrElse(8)

    val amountDecimals = getDecimals(lo.order.assetPair.amountAsset)
    val priceDecimals = getDecimals(lo.order.assetPair.priceAsset)
    val price = BigDecimal(lo.price)*math.pow(10, amountDecimals - priceDecimals)/Order.PriceConstant

    val scaled = if (price >= 1) {
        val amount = (BigInt(lo.amount) * lo.price / Order.PriceConstant).bigInteger.longValueExact()
        BigDecimal(amount, priceDecimals)
      } else {
        BigDecimal(lo.amount, amountDecimals)
      }

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
