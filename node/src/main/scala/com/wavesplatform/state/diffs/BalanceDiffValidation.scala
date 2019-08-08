package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.AccountBalanceError
import com.wavesplatform.utils.ScorexLogging

import scala.util.{Left, Right}

object BalanceDiffValidation extends ScorexLogging {

  def apply(b: Blockchain, currentHeight: Int, fs: FunctionalitySettings)(d: Diff): Either[AccountBalanceError, Diff] = {
    val changedAccounts = d.portfolios.keySet

    def check(acc: Address): Option[(Address, String)] = {
      val portfolioDiff = d.portfolios(acc)

      val balance       = portfolioDiff.balance
      lazy val oldWaves = b.balance(acc, Waves)
      lazy val oldLease = b.leaseBalance(acc)
      lazy val lease    = cats.Monoid.combine(oldLease, portfolioDiff.lease)
      (if (balance < 0) {
         val newB = oldWaves + balance

         if (newB < 0) {
           Some(acc -> s"negative waves balance: $acc, old: $oldWaves, new: $newB")
         } else if (newB < lease.out && currentHeight > fs.allowLeasedBalanceTransferUntilHeight) {
           Some(acc -> (if (newB + lease.in - lease.out < 0) {
                          s"negative effective balance: $acc, old: ${(oldWaves, oldLease)}, new: ${(newB, lease)}"
                        } else if (portfolioDiff.lease.out == 0) {
                          s"$acc trying to spend leased money"
                        } else {
                          s"leased being more than own: $acc, old: ${(oldWaves, oldLease)}, new: ${(newB, lease)}"
                        }))
         } else {
           None
         }
       } else {
         None
       }) orElse (portfolioDiff.assets find {
        case (a, c) =>
          // Tokens it can produce overflow are exist.
          val oldB = b.balance(acc, a)
          val newB = oldB + c
          newB < 0
      } map { _ =>
        acc -> s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(Distributions(b).portfolio(acc).combine(portfolioDiff))}"
      })
    }

    val positiveBalanceErrors: Map[Address, String] = changedAccounts.flatMap(check).toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def negativeAssetsInfo(p: Portfolio): Map[ByteStr, Long] =
    p.assets.collect {
      case (asset, balance) if balance < 0 => (asset.id, balance)
    }
}
