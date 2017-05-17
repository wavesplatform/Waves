package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.TransactionValidationError

import scala.util.{Left, Right}

object BalanceDiffValidation {
  def apply[T <: Transaction](s: StateReader)(tx: T, d: Diff): Either[TransactionValidationError, Diff] = {

    val changedAccounts = d.portfolios.keySet
    val positiveBalanceErrors = changedAccounts.flatMap(acc => {

      val oldPortfolio = s.accountPortfolio(acc)
      val portfolioDiff = d.portfolios(acc)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)

      val allBalancesAndAssetsForAccountArePositive = newPortfolio.balance >= 0 &&
        newPortfolio.balance >= newPortfolio.leaseInfo.leaseOut &&
        newPortfolio.effectiveBalance >= 0 &&
        newPortfolio.assets.values.forall(_ >= 0)
      if (!allBalancesAndAssetsForAccountArePositive) {
        Some(s"$acc, old: $oldPortfolio, new: $newPortfolio")
      } else None

    })
    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(TransactionValidationError(tx, s"Transaction application leads to negative" +
        s" balance/assetBalance or leased being more than own: $positiveBalanceErrors"))
    }
  }
}
