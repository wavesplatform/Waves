package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.account.Address
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation {


  def apply[T <: Transaction](s: StateReader, time: Long, fn: Functionalities)(d: Diff): Either[AccountBalanceError, Diff] = {

    val changedAccounts = d.portfolios.keySet
    val positiveBalanceErrors: Map[Address, String] = changedAccounts.flatMap(acc => {

      val oldPortfolio = s.accountPortfolio(acc)
      val portfolioDiff = d.portfolios(acc)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)

      val err = if (newPortfolio.balance < 0) {
        Some(s"negative waves balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
      } else if (newPortfolio.assets.values.exists(_ < 0)) {
        Some(s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(newPortfolio)}")
      } else if (newPortfolio.effectiveBalance < 0) {
        Some(s"negative effective balance: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(oldPortfolio)}")
      } else if (newPortfolio.balance < newPortfolio.leaseInfo.leaseOut && fn.allowLeasedBalanceTransferUpTo.check(time).isLeft) {
        Some(s"leased being more than own: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(oldPortfolio)}")
      } else None
      err.map(acc -> _)
    }).toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def leaseWavesInfo(p: Portfolio): (Long, LeaseInfo) = (p.balance, p.leaseInfo)

  private def negativeAssetsInfo(p: Portfolio): Map[ByteStr, Long] = p.assets.filter(_._2 < 0)
}
