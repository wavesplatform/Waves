package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation {

  def apply(b: Blockchain)(d: Diff): Either[ValidationError, Diff] = {
    def check(acc: Address, portfolio: Portfolio): Either[(Address, String), Unit] = {
      val balance  = portfolio.balance
      val oldWaves = b.balance(acc, Waves)
      val oldLease = b.leaseBalance(acc)

      def negativeBalanceCheck(newLease: LeaseBalance, newWaves: Long): Either[(Address, String), Unit] =
        if (balance < 0) {
          if (newWaves < 0) {
            Left(acc -> s"negative waves balance: $acc, old: $oldWaves, new: $newWaves")
          } else if (newWaves < newLease.out && b.height > b.settings.functionalitySettings.allowLeasedBalanceTransferUntilHeight) {
            Left(acc -> (if (newWaves + newLease.in - newLease.out < 0) {
                           s"negative effective balance: $acc, old: ${(oldWaves, oldLease)}, new: ${(newWaves, newLease)}"
                         } else if (portfolio.lease.out == 0) {
                           s"$acc trying to spend leased money"
                         } else {
                           s"leased being more than own: $acc, old: ${(oldWaves, oldLease)}, new: ${(newWaves, newLease)}"
                         }))
          } else {
            Right(())
          }
        } else {
          Right(())
        }

      // Tokens it can produce overflow are exist.
      lazy val assetsCheck =
        portfolio.assets
          .collectFirst {
            case (asset, diffAmount) if b.balance(acc, asset) + diffAmount < 0 =>
              Left(acc -> s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(b, acc, portfolio)}")
          }
          .getOrElse(Right(()))

      for {
        newLease <- oldLease.combineF[Either[String, *]](portfolio.lease).leftMap((acc, _))
        newWaves <- state.safeSum(oldWaves, balance, "Waves balance").leftMap((acc, _))
        _        <- negativeBalanceCheck(newLease, newWaves)
        _        <- assetsCheck
      } yield ()
    }

    val positiveBalanceErrors =
      d.portfolios.flatMap { case (acc, p) => check(acc, p).fold(error => List(error), _ => Nil) }

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def negativeAssetsInfo(b: Blockchain, acc: Address, diff: Portfolio): Map[ByteStr, Long] =
    diff.assets
      .map { case (aid, balanceChange) => aid.id -> (b.balance(acc, aid) + balanceChange) }
      .filter(_._2 < 0)
}
