package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{Blockchain, ByteStr, Diff, Portfolio}
import com.wavesplatform.transaction.ValidationError.AccountBalanceError
import com.wavesplatform.utils.ScorexLogging

import scala.util.{Left, Right}

object BalanceDiffValidation extends ScorexLogging with Instrumented {

  def apply(b: Blockchain, currentHeight: Int, fs: FunctionalitySettings)(d: Diff): Either[AccountBalanceError, Diff] = {
    val changedAccounts = d.portfolios.keySet

    def check(acc: Address): Option[(Address, String)] = {
      val portfolioDiff = d.portfolios(acc)

//      val oldPortfolio = b.portfolio(acc)                    //Debug
//      val newPortfolio = oldPortfolio.combine(portfolioDiff) //Debug

      val balance       = portfolioDiff.balance
      lazy val oldWaves = b.balance(acc, None)
      lazy val oldLease = b.leaseBalance(acc)
      lazy val lease    = cats.Monoid.combine(oldLease, portfolioDiff.lease)
      if (balance < 0) {
        val newB = oldWaves + balance

//        println(b.getClass())
//        println(s"new balance $newB ${newPortfolio.balance}")
//        println(s"new lease   ${oldLease.out} ${oldPortfolio.lease.out} ${lease.out} ${newPortfolio.lease.out}")

        if (newB < 0) {
          return Some(acc -> s"negative waves balance: $acc, old: ${oldWaves}, new: ${newB}")
        }
        if (newB < lease.out && currentHeight > fs.allowLeasedBalanceTransferUntilHeight) {
          return Some(acc -> (if (newB + lease.in - lease.out < 0) {
                                s"negative effective balance: $acc, old: ${(oldWaves, oldLease)}, new: ${(newB, lease)}"
                              } else if (portfolioDiff.lease.out == 0) {
                                s"$acc trying to spend leased money"
                              } else {
                                s"leased being more than own: $acc, old: ${(oldWaves, oldLease)}, new: ${(newB, lease)}"
                              }))
        }
      }
      for ((a, c) <- portfolioDiff.assets) {
        // Tokens it can produce overflow are exist.
        val oldB = b.balance(acc, Some(a))
        val newB = oldB + c
        if (newB < 0) {
//          println(s"found negative asset balance: $newB $a $c $oldB ${b.portfolio(acc)}")
          return Some(acc -> s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(b.portfolio(acc).combine(portfolioDiff))}")
        }
      }
      return None
    }

    val positiveBalanceErrors: Map[Address, String] = changedAccounts.flatMap(check).toMap
//       if (portfolioDiff.balance < 0 || portfolioDiff.assets.values
//              .exists(_ < 0) || portfolioDiff.effectiveBalance < 0 || portfolioDiff.balance < portfolioDiff.lease.out) {
//          val oldPortfolio = b.portfolio(acc)
//
//          val newPortfolio = oldPortfolio.combine(portfolioDiff)
//
//          lazy val negativeBalance          = newPortfolio.balance < 0
//          lazy val negativeAssetBalance     = newPortfolio.assets.values.exists(_ < 0)
//          lazy val negativeEffectiveBalance = newPortfolio.effectiveBalance < 0
//          lazy val leasedMoreThanOwn        = newPortfolio.balance < newPortfolio.lease.out && currentHeight > fs.allowLeasedBalanceTransferUntilHeight
//
//          val err = if (negativeBalance) {
//            Some(s"negative waves balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
//          } else if (negativeAssetBalance) {
//            Some(s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(newPortfolio)}")
//          } else if (negativeEffectiveBalance) {
//            Some(s"negative effective balance: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(newPortfolio)}")
//          } else if (leasedMoreThanOwn && oldPortfolio.lease.out == newPortfolio.lease.out) {
//            Some(s"$acc trying to spend leased money")
//          } else if (leasedMoreThanOwn) {
//            Some(s"leased being more than own: $acc, old: ${leaseWavesInfo(oldPortfolio)}, new: ${leaseWavesInfo(newPortfolio)}")
//          } else None
//          err.map(acc -> _)
//        } else {
//          None
//        }
//      }

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def negativeAssetsInfo(p: Portfolio): Map[ByteStr, Long] = p.assets.filter(_._2 < 0)
}
