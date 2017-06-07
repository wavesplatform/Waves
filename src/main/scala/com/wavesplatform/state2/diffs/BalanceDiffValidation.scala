package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, LeaseInfo, Portfolio}
import scorex.account.Account
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.AccountsValidationError

import scala.util.{Left, Right}

object BalanceDiffValidation {


  def apply[T <: Transaction](s: StateReader, time: Long, fs: FunctionalitySettings)(tx: T, d: Diff): Either[AccountsValidationError, Diff] = {

    val changedAccounts = d.portfolios.keySet
    val positiveBalanceErrors = changedAccounts.flatMap(acc => {

      val oldPortfolio = s.accountPortfolio(acc)
      val portfolioDiff = d.portfolios(acc)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)


      if (newPortfolio.balance < 0) {
        Seq(acc -> s"negative waves balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
      } else if (newPortfolio.assets.values.exists(_ < 0)) {
        Seq(acc -> s"negative asset balance: $acc, old: $oldPortfolio, new: $newPortfolio")
      } else if (newPortfolio.effectiveBalance < 0) {
        Seq(acc -> s"negative effective balance: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(oldPortfolio)}")
      } else if (newPortfolio.balance < newPortfolio.leaseInfo.leaseOut && time > fs.allowLeasedBalanceTransferUntil) {
        Seq(acc -> s"leased being more than own: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(oldPortfolio)}")
      } else Seq.empty[(Account, String)]
    })

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountsValidationError(positiveBalanceErrors.map(e => (e._1, s"Transaction application leads to $e._2"))))
    }
  }

  private def leaseWavesInfo(p: Portfolio): (Long, LeaseInfo) = (p.balance, p.leaseInfo)

}
