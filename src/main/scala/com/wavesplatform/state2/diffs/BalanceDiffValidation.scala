package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff}
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.TransactionValidationError

import scala.util.{Left, Right}

object BalanceDiffValidation {
  def apply[T <: Transaction](s: StateReader, time: Long)(tx: T, d: Diff): Either[TransactionValidationError, Diff] = {

    val changedAccounts = d.portfolios.keySet
    val positiveBalanceErrors = changedAccounts.flatMap(acc => {

      val oldPortfolio = s.accountPortfolio(acc)
      val portfolioDiff = d.portfolios(acc)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)

      if(acc.address=="3PESyRqYseiNymihU6PQErafFGyDEDUMVe1") {
        println(s"!!UP2DATE old: $oldPortfolio,  diff: $portfolioDiff, new: $newPortfolio")
      }


      val allBalancesAndAssetsForAccountArePositive = newPortfolio.balance >= 0 &&
        newPortfolio.effectiveBalance >= 0 &&
        newPortfolio.assets.values.forall(_ >= 0)
      if (!allBalancesAndAssetsForAccountArePositive) {
        Some(s"$acc, old: $oldPortfolio, new: $newPortfolio")
      } else None

    })
    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(TransactionValidationError(tx, s"Transaction application leads to (temporary) negative" +
        s" balance/effectiveBalance/assetBalance : $positiveBalanceErrors"))
    }
  }
}
