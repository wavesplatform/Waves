package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, LeaseBalance, StateSnapshot}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation {
  def cond(b: Blockchain, cond: Blockchain => Boolean)(s: StateSnapshot): Either[AccountBalanceError, StateSnapshot] = {
    if (cond(b)) apply(b)(s)
    else Right(s)
  }

  def apply(b: Blockchain)(snapshot: StateSnapshot): Either[AccountBalanceError, StateSnapshot] = {
    def checkWaves(
        acc: Address,
        newWaves: Long,
        newLease: LeaseBalance
    ): Either[(Address, String), Unit] = {
      val oldWaves     = b.balance(acc)
      val oldLease     = b.leaseBalance(acc)
      val wavesDiff    = newWaves - oldWaves
      val leaseOutDiff = newLease.out - oldLease.out

      if (wavesDiff < 0) {
        if (newWaves < 0) {
          Left(acc -> s"negative waves balance: $acc, old: $oldWaves, new: $newWaves")
        } else if (newWaves < newLease.out && b.height > b.settings.functionalitySettings.allowLeasedBalanceTransferUntilHeight) {
          val errorMessage =
            if (newWaves + newLease.in - newLease.out < 0)
              s"negative effective balance: $acc, old: ${(oldWaves, oldLease)}, new: ${(newWaves, newLease)}"
            else if (leaseOutDiff == 0)
              s"$acc trying to spend leased money"
            else
              s"leased being more than own: $acc, old: ${(oldWaves, oldLease)}, new: ${(newWaves, newLease)}"
          Left(acc -> errorMessage)
        } else {
          Right(())
        }
      } else {
        Right(())
      }
    }

    val wavesCheck =
      snapshot.balances
        .flatMap {
          case ((address, Waves), balance) =>
            val currentLeaseBalance = snapshot.leaseBalances.getOrElse(address, b.leaseBalance(address))
            checkWaves(address, balance, currentLeaseBalance).fold(error => List(error), _ => Nil)
          case _ =>
            Nil
        }

    val assetsCheck =
      snapshot.balances
        .collectFirst {
          case ((address, asset), balance) if asset != Waves && balance < 0 =>
            Map(address -> s"negative asset balance: $address, new portfolio: ${negativeAssetsInfo(address, snapshot)}")
        }
        .getOrElse(Map())

    val positiveBalanceErrors =
      wavesCheck ++ assetsCheck

    if (positiveBalanceErrors.isEmpty) {
      Right(snapshot)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def negativeAssetsInfo(
      address: Address,
      snapshot: StateSnapshot
  ): Map[ByteStr, Long] =
    snapshot.balances
      .collect {
        case ((`address`, assetId: IssuedAsset), balance) if balance < 0 => (assetId.id, balance)
      }
}
