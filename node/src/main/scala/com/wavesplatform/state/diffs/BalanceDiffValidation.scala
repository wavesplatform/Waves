package com.wavesplatform.state.diffs

import cats.implicits.*
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.CurrentBalance
import com.wavesplatform.state
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation {
  def cond(b: Blockchain, cond: Blockchain => Boolean)(d: Diff): Either[AccountBalanceError, Diff] = {
    if (cond(b)) apply(b)(d)
    else Right(d)
  }

  def apply(b: Blockchain)(d: Diff): Either[AccountBalanceError, Diff] = {
    def check(
        acc: Address,
        portfolio: Portfolio
//        oldBalances: Map[Address, Long],
//        oldLeases: Map[Address, LeaseBalance]
    ): Either[(Address, String), Unit] = {
      val balance  = portfolio.balance
      val oldWaves = b.balance(acc, Waves) // oldBalances.getOrElse(acc, CurrentBalance.Unavailable.balance)
      val oldLease = b.leaseBalance(acc)   // oldLeases.getOrElse(acc, LeaseBalance.empty)

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

//    val oldBalances = b.wavesBalances(d.portfolios.keys.toSeq)
//    val oldLeases   = b.leaseBalances(d.portfolios.keys.toSeq)

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
