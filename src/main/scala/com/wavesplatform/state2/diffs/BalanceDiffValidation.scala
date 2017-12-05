package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.account.Address
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.AccountBalanceError
import scorex.utils.ScorexLogging

import scala.util.{Left, Right}

object BalanceDiffValidation extends ScorexLogging with Instrumented {

  def apply[T <: Transaction](s: SnapshotStateReader, fs: FunctionalitySettings)(d: Diff): Either[AccountBalanceError, Diff] = {

    val changedAccounts = d.portfolios.keySet

    val positiveBalanceErrors: Map[Address, String] = changedAccounts.flatMap(acc => {

      val portfolioDiff = d.portfolios(acc)
      val oldPortfolio = s.partialPortfolio(acc, portfolioDiff.assets.keySet)
      val newPortfolio = oldPortfolio.combine(portfolioDiff)

      val err = if (newPortfolio.balance < 0) {
        Some(s"negative waves balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
      } else if (newPortfolio.assets.values.exists(_ < 0)) {
        Some(s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(newPortfolio)}")
      } else if (newPortfolio.effectiveBalance < 0) {
        Some(s"negative effective balance: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(newPortfolio)}")
      } else if (newPortfolio.balance < newPortfolio.leaseInfo.leaseOut && s.height > fs.allowLeasedBalanceTransferUntilHeight) {
        Some(s"leased being more than own: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(newPortfolio)}")
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
