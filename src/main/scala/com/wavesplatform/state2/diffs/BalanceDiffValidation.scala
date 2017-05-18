package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.TransactionValidationError

import scala.util.{Left, Right}

object BalanceDiffValidation {


  def apply[T <: Transaction](s: StateReader, time: Long, fs: FunctionalitySettings)(tx: T, d: Diff): Either[TransactionValidationError, Diff] = {

    val changedAccounts = d.portfolios.keySet
    val positiveBalanceErrors = changedAccounts.flatMap(acc => {

      val oldPortfolio = s.accountPortfolio(acc)
      val portfolioDiff = d.portfolios(acc)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)


      if (newPortfolio.balance < 0) {
        Some(s"negative waves balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
      } else if (newPortfolio.assets.values.exists(_ < 0)) {
        Some(s"negative asset balance: $acc, old: $oldPortfolio, new: $newPortfolio")
      } else if (newPortfolio.effectiveBalance < 0) {
        Some(s"negative effective balance: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(oldPortfolio)}")
      } else if (newPortfolio.balance < newPortfolio.leaseInfo.leaseOut && time > fs.allowTransferLeasedBalanceUntil) {
        Some(s"leased being more than own: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(oldPortfolio)}")
      } else None

    })
    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(TransactionValidationError(tx, s"Transaction application leads to $positiveBalanceErrors"))
    }
  }

  private def leaseWavesInfo(p: Portfolio): (Long, LeaseInfo) = (p.balance, p.leaseInfo)

}
