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
  def apply(settings: FunctionalitySettings, time: Long, h: Int)(s: StateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- GeneralValidation(s, settings, time, tx)
      t1 <- PaymentTransactionIncrementingTimestampValidation(s,settings)(t0)
      t2 <- ReissueBurnTransactionsValidation(s)(t1)
    } yield {
      t2 match {
        case gtx: GenesisTransaction =>
          Diff(height = h,
            tx = gtx,
            portfolios = Map(gtx.recipient -> Portfolio(
              balance = gtx.amount,
              effectiveBalance = gtx.amount,
              assets = Map.empty)),
            issuedAssets = Map.empty)
        case ptx: PaymentTransaction => {
          Diff(height = h,
            tx = ptx,
            portfolios = Map(
              ptx.recipient -> Portfolio(
                balance = ptx.amount,
                effectiveBalance = ptx.amount,
                assets = Map.empty)) combine Map(
              Account.fromPublicKey(ptx.sender.publicKey) -> Portfolio(
                balance = -ptx.amount - ptx.fee,
                effectiveBalance = -ptx.amount - ptx.fee,
                assets = Map.empty
              )
            ),
            issuedAssets = Map.empty
          )
        }
      }
    }
  }
}

