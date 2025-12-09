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
      val oldWaves   = b.balance(acc)
      val oldDeposit = b.generationDeposit(acc)
      val oldLease   = b.leaseBalance(acc)

      val newDeposit          = oldDeposit + additionalDeposit
      val newWavesWithDeposit = newWaves - newDeposit

      val leaseOutDiff = newLease.out - oldLease.out

      val stateChanges = s"old: w=$oldWaves, $oldLease, d=$oldDeposit, new: w=$newWaves, $newLease, d=$newDeposit"

      // TODO: additional tests
      val errorMessage =
        if (newWaves < 0) s"negative waves balance: $acc, old: $oldWaves, new: $newWaves".asLeft
        else if (newWavesWithDeposit < 0) {
          if (newDeposit > oldDeposit) s"$acc not enough funds for deposit, $stateChanges".asLeft
          else s"$acc trying to spend a deposit, $stateChanges".asLeft
        } else if (newWavesWithDeposit < newLease.out && b.height > b.settings.functionalitySettings.allowLeasedBalanceTransferUntilHeight) {
          if (newWavesWithDeposit + newLease.in - newLease.out < 0) s"negative effective balance: $acc, $stateChanges".asLeft
          else if (leaseOutDiff == 0) s"$acc trying to spend leased money".asLeft
          else s"leased being more than own: $acc, $stateChanges".asLeft
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
