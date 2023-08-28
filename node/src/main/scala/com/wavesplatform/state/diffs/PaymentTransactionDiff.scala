package com.wavesplatform.state.diffs

import cats.implicits.toBifunctorOps
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Portfolio, StateSnapshot}
import com.wavesplatform.transaction.PaymentTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

object PaymentTransactionDiff {

  def apply(blockchain: Blockchain)(tx: PaymentTransaction): Either[ValidationError, StateSnapshot] = {
    val blockVersion3AfterHeight = blockchain.settings.functionalitySettings.blockVersion3AfterHeight
    if (blockchain.height > blockVersion3AfterHeight)
      Left(GenericError(s"Payment transaction is deprecated after h=$blockVersion3AfterHeight"))
    else {
      for {
        portfolios <- Portfolio.combine(
          Map(tx.recipient        -> Portfolio(tx.amount.value)),
          Map(tx.sender.toAddress -> Portfolio(-tx.amount.value - tx.fee.value))
        ).leftMap(GenericError(_))
        snapshot <- StateSnapshot.build(blockchain, portfolios)
      } yield snapshot
    }
  }
}
