package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{Diff, Portfolio}
import scorex.account.{Account, Alias}
import scorex.transaction.StateValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

import scala.util.{Left, Right}

object LeaseTransactionsDiff {

  def lease(s: StateReader, height: Int)(tx: LeaseTransaction): Either[StateValidationError, Diff] = {
    val sender = Account.fromPublicKey(tx.sender.publicKey)
    val recipient = tx.recipient match {
      case a: Account => a
      case a: Alias => ???
    }
    val portfolioDiff: Map[Account, Portfolio] = Map(
      sender -> Portfolio(-tx.fee, -tx.fee - tx.amount, Map.empty),
      recipient -> Portfolio(0, tx.amount, Map.empty)
    )
    Right(Diff(height = height, tx = tx, portfolios = portfolioDiff, assetInfos = Map.empty))
  }

  def leaseCancel(s: StateReader, height: Int)(tx: LeaseCancelTransaction): Either[StateValidationError, Diff] = {
    val leaseOpt = s.findTransaction[LeaseTransaction](tx.leaseId)
    leaseOpt match {
      case Some(leaseTx) if leaseTx.sender.publicKey.sameElements(leaseTx.sender.publicKey) =>
        val sender = Account.fromPublicKey(tx.sender.publicKey)
        val recipient = leaseTx.recipient match {
          case a: Account => a
          case a: Alias => ???
        }
        val portfolioDiff: Map[Account, Portfolio] = Map(
          sender -> Portfolio(-tx.fee, -tx.fee + leaseTx.amount, Map.empty),
          recipient -> Portfolio(0, -leaseTx.amount, Map.empty)
        )
        Right(Diff(height = height, tx = tx, portfolios = portfolioDiff, assetInfos = Map.empty))
      case Some(leaseTx) => Left(TransactionValidationError(tx, s"LeaseTransaction was leased by other sender"))
      case None => Left(TransactionValidationError(tx, s"Related LeaseTransaction not found"))
    }
  }
}
