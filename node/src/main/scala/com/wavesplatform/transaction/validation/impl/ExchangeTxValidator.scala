package com.wavesplatform.transaction.validation.impl

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order, OrderType}
import com.wavesplatform.transaction.validation.TxValidator
import com.wavesplatform.transaction.{PBSince, TxVersion}

object ExchangeTxValidator extends TxValidator[ExchangeTransaction] {
  override def validate(tx: ExchangeTransaction): ValidatedNel[ValidationError, ExchangeTransaction] = {
    import tx.*

    V.seq(tx)(
      V.cond(sellMatcherFee <= Order.MaxAmount, GenericError("sellMatcherFee too large")),
      V.cond(buyMatcherFee <= Order.MaxAmount, GenericError("buyMatcherFee too large")),
      V.cond(fee.value <= Order.MaxAmount, GenericError("fee too large")),
      V.cond(PBSince.affects(tx) || order1.orderType == OrderType.BUY, GenericError("order1 should have OrderType.BUY")),
      V.cond(buyOrder.orderType == OrderType.BUY, GenericError("buyOrder should has OrderType.BUY")),
      V.cond(sellOrder.orderType == OrderType.SELL, GenericError("sellOrder should has OrderType.SELL")),
      V.cond(buyOrder.matcherPublicKey == sellOrder.matcherPublicKey, GenericError("buyOrder.matcher should be the same as sellOrder.matcher")),
      V.cond(buyOrder.assetPair == sellOrder.assetPair, GenericError("Both orders should have same AssetPair")),
      V.cond(buyOrder.isValid(timestamp), OrderValidationError(buyOrder, buyOrder.isValid(timestamp).messages())),
      V.cond(sellOrder.isValid(timestamp), OrderValidationError(sellOrder, sellOrder.isValid(timestamp).messages())),
      V.cond(
        version > TxVersion.V1 || (buyOrder.version == Order.V1 && sellOrder.version == Order.V1),
        GenericError("can only contain orders of version 1")
      ),
      V.cond(
        version > TxVersion.V2 || (buyOrder.version < Order.V4 && sellOrder.version < Order.V4),
        GenericError("can only contain orders of version < 4")
      )
    )
  }
}
