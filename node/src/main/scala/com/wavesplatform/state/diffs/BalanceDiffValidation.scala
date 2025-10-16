package com.wavesplatform.state.diffs

import cats.syntax.either.*
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, LeaseBalance, StateSnapshot}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.CommitToGenerationTransaction.DepositInWavelets
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
        newLease: LeaseBalance,
        additionalDeposit: Long
    ): Either[(Address, String), Unit] = {
      val oldWaves            = b.balance(acc)
      val oldDeposit          = b.deposit(acc)
      val oldWavesWithDeposit = oldWaves - oldDeposit
      val oldLease            = b.leaseBalance(acc)

      val newDeposit          = oldDeposit + additionalDeposit
      val newWavesWithDeposit = newWaves - newDeposit

      val wavesDiff    = newWavesWithDeposit - oldWavesWithDeposit
      val leaseOutDiff = newLease.out - oldLease.out

      val errorMessage =
        if (wavesDiff >= 0) Either.unit
        else if (newWavesWithDeposit < 0) {
          if (newDeposit > oldDeposit)
            s"$acc not enough funds for deposit, old: ${(oldWaves, oldLease, oldDeposit)}, new: ${(newWaves, newLease, newDeposit)}".asLeft
          else if (oldDeposit > 0)
            s"$acc trying to spend a deposit, old: ${(oldWaves, oldLease, oldDeposit)}, new: ${(newWaves, newLease, newDeposit)}".asLeft
          else s"negative waves balance: $acc, old: $oldWaves, new: $newWaves".asLeft
        } else if (newWavesWithDeposit < newLease.out && b.height > b.settings.functionalitySettings.allowLeasedBalanceTransferUntilHeight) {
          if (newWavesWithDeposit + newLease.in - newLease.out < 0)
            s"negative effective balance: $acc, old: ${(oldWaves, oldLease, oldDeposit)}, new: ${(newWaves, newLease, newDeposit)}".asLeft
          else if (leaseOutDiff == 0) s"$acc trying to spend leased money".asLeft
          else s"leased being more than own: $acc, old: ${(oldWaves, oldLease, oldDeposit)}, new: ${(newWaves, newLease, newDeposit)}".asLeft
        } else Either.unit

      errorMessage.leftMap(acc -> _)
    }

    val wavesCheck =
      snapshot.balances
        .flatMap {
          case ((address, Waves), balance) =>
            val currentLeaseBalance = snapshot.leaseBalances.getOrElse(address, b.leaseBalance(address))
            val depositedOnNext = DepositInWavelets *
              snapshot.nextCommittedGenerators.find { case (pk, _) => pk.toAddress == address }.size
            checkWaves(address, balance, currentLeaseBalance, depositedOnNext).fold(error => List(error), _ => Nil)
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
