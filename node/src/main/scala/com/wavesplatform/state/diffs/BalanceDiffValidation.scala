package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxValidationError.{AccountBalanceError, GenericError}
import com.wavesplatform.utils.ScorexLogging

object BalanceDiffValidation extends ScorexLogging {

  def apply(b: Blockchain)(d: Diff): Either[ValidationError, Diff] = {
    val changedAccounts = d.portfolios.keySet

    def check(acc: Address): Either[String, Option[(Address, String)]] = {
      val portfolioDiff = d.portfolios(acc)

      val balance       = portfolioDiff.balance
      lazy val oldWaves = b.balance(acc, Waves)
      lazy val oldLease = b.leaseBalance(acc)
      oldLease
        .combine(portfolioDiff.lease)
        .map(
          lease =>
            (if (balance < 0) {
               val newB = oldWaves + balance

               if (newB < 0) {
                 Some(acc -> s"negative waves balance: $acc, old: $oldWaves, new: $newB")
               } else if (newB < lease.out && b.height > b.settings.functionalitySettings.allowLeasedBalanceTransferUntilHeight) {
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
              acc -> s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(b, acc, portfolioDiff)}"
            })
        )
    }

    changedAccounts.toList
      .flatTraverse(check(_).map(_.toList))
      .leftMap(GenericError(_))
      .flatMap(balanceErrors => Either.cond(balanceErrors.isEmpty, d, AccountBalanceError(balanceErrors.toMap)))
  }

  private def negativeAssetsInfo(b: Blockchain, acc: Address, diff: Portfolio): Map[ByteStr, Long] =
    diff.assets
      .map { case (aid, balanceChange) => aid.id -> (b.balance(acc, aid) + balanceChange) }
      .filter(_._2 < 0)
}
