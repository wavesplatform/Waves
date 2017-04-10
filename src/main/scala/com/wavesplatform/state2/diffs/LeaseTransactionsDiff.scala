package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2._
import scorex.account.{Account, Alias}
import scorex.transaction.StateValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

import scala.util.{Left, Right}

object LeaseTransactionsDiff {

  def lease(s: StateReader, height: Int)(tx: LeaseTransaction): Either[StateValidationError, Diff] = {
    val sender = Account.fromPublicKey(tx.sender.publicKey)
    s.resolveAliasEi(tx.recipient).flatMap { recipient =>
      if (recipient == sender)
        Left(TransactionValidationError(tx, "Cannot lease to self"))
      else {
        val ap = s.accountPortfolio(tx.sender)
        if (ap.balance - ap.leaseInfo.leaseOut < tx.amount) {
          Left(TransactionValidationError(tx, s"Cannot lease more than own: Balance:${ap.balance}, already leased: ${ap.leaseInfo.leaseOut}"))
        }
        else {
          val portfolioDiff: Map[Account, Portfolio] = Map(
            sender -> Portfolio(-tx.fee, LeaseInfo(0, tx.amount), Map.empty),
            recipient -> Portfolio(0, LeaseInfo(tx.amount, 0), Map.empty)
          )
          Right(Diff(height = height, tx = tx, portfolios = portfolioDiff))
        }
      }
    }
  }

  def leaseCancel(s: StateReader, height: Int)(tx: LeaseCancelTransaction): Either[StateValidationError, Diff] = {
    val leaseOpt = s.findTransaction[LeaseTransaction](tx.leaseId)
    leaseOpt match {
      case Some(leaseTx) if tx.sender != leaseTx.sender => Left(TransactionValidationError(tx, s"LeaseTransaction was leased by other sender"))
      case None => Left(TransactionValidationError(tx, s"Related LeaseTransaction not found"))
      case Some(leaseTx) if !s.isLeaseActive(leaseTx)=> Left(TransactionValidationError(tx, s"Cannot cancel already cancelled lease"))
      case Some(leaseTx) =>
        val sender = Account.fromPublicKey(tx.sender.publicKey)
        s.resolveAliasEi(leaseTx.recipient).map { recipient =>
          val portfolioDiff: Map[Account, Portfolio] = Map(
            sender -> Portfolio(-tx.fee, LeaseInfo(0, -leaseTx.amount), Map.empty),
            recipient -> Portfolio(0, LeaseInfo(-leaseTx.amount, 0), Map.empty)
          )
          Diff(height = height, tx = tx, portfolios = portfolioDiff)
        }
    }
  }
}
