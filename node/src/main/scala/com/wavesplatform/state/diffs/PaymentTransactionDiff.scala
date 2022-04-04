package com.wavesplatform.state.diffs

import cats.implicits.toBifunctorOps
import com.wavesplatform.account.Address
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.PaymentTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Left

object PaymentTransactionDiff {

  def apply(blockchain: Blockchain)(tx: PaymentTransaction): Either[ValidationError, Diff] = {
    val blockVersion3AfterHeight = blockchain.settings.functionalitySettings.blockVersion3AfterHeight
    if (blockchain.height > blockVersion3AfterHeight)
      Left(GenericError(s"Payment transaction is deprecated after h=$blockVersion3AfterHeight"))
    else
      Diff
        .combine(
          Map(tx.recipient -> Portfolio(balance = tx.amount.value, LeaseBalance.empty, assets = Map.empty)),
          Map(
            Address.fromPublicKey(tx.sender) -> Portfolio(
              balance = -tx.amount.value - tx.fee.value,
              LeaseBalance.empty,
              assets = Map.empty
            )
          )
        )
        .bimap(GenericError(_), p => Diff(portfolios = p))
  }
}
