package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff}
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.TransactionValidationError

import scala.util.{Left, Right}

object BalanceDiffValidation {
  def apply[T <: Transaction](s: StateReader, settings: FunctionalitySettings, time: Long)(tx: T, d: Diff): Either[TransactionValidationError, Diff] = {

    lazy val leadsToPositiveState: Boolean = {
      val changedAccounts = d.portfolios.keySet
      val allBalancesArePositive = changedAccounts.toStream.forall(acc => {
        val oldPortfolio = s.accountPortfolio(acc)
        val portfolioDiff = d.portfolios(acc)
        val newPortfolio = oldPortfolio.combine(portfolioDiff)
        val allBalancesAndAssetsForAccountArePositive = newPortfolio.balance >= 0 &&
          newPortfolio.effectiveBalance >= 0 &&
          newPortfolio.assets.values.forall(_ >= 0)

        allBalancesAndAssetsForAccountArePositive
      })
      allBalancesArePositive
    }

    if (time >= settings.allowTemporaryNegativeUntil || leadsToPositiveState) {
      Right(d)
    } else {
      Left(TransactionValidationError(tx, "Transaction application leads to temporary negative balance"))
    }
  }
}
