package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{BlockDiff, CompositeStateReader, Diff, EqByteArray, Portfolio, StateReader}
import scorex.account.{Account, Alias}
import scorex.block.Block
import scorex.transaction.ValidationError.{AliasNotExists, TransactionValidationError}
import scorex.transaction._
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.utils.ByteArray

import scala.util.{Left, Right}

object TransactionDiffer {
  def apply(settings: FunctionalitySettings, time: Long, height: Int)(s: StateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      transaction <- GeneralValidation(s, settings, time, tx)
      diff <- transaction match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(height)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, settings, height)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(s, height)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, height)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, height)(btx)
      }
    } yield diff
  }
}

object BalanceDiffValidation {
  def apply[T <: Transaction](s: StateReader, settings: FunctionalitySettings)(tx: T, d: Diff): Either[TransactionValidationError, Diff] = {
    ???
  }
}
