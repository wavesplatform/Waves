package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import scorex.account.Account
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

import scala.util.{Left, Right}

object LeaseTransactionsDiff {

  def lease(s: StateReader, height: Int)(tx: LeaseTransaction): Either[ValidationError, Diff] = {
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
          Right(Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(tx.id -> true)))
        }
      }
    }
  }

  def leaseCancel(s: StateReader, settings: FunctionalitySettings, time: Long, height: Int)
                 (tx: LeaseCancelTransaction): Either[ValidationError, Diff] = {
    val leaseEi = s.findTransaction[LeaseTransaction](tx.leaseId) match {
      case None => Left(TransactionValidationError(tx, s"Related LeaseTransaction not found"))
      case Some(l) => Right(l)
    }
    for {
      lease <- leaseEi
      recipient <- s.resolveAliasEi(lease.recipient)
      isLeaseActive = s.isLeaseActive(lease)
      _ <- if (!isLeaseActive && time > settings.allowMultipleLeaseCancelTransactionUntilTimestamp)
        Left(TransactionValidationError(tx, s"Cannot cancel already cancelled lease")) else Right(())
      canceller = Account.fromPublicKey(tx.sender.publicKey)
      portfolioDiff <- if (tx.sender == lease.sender) {
        Right(Monoid.combine(
          Map(canceller -> Portfolio(-tx.fee, LeaseInfo(0, -lease.amount), Map.empty)),
          Map(recipient -> Portfolio(0, LeaseInfo(-lease.amount, 0), Map.empty))))
      } else if (time < settings.allowMultipleLeaseCancelTransactionUntilTimestamp) { // cancel of another acc
        Right(Monoid.combine(
          Map(canceller -> Portfolio(-tx.fee, LeaseInfo(0, -lease.amount), Map.empty)),
          Map(recipient -> Portfolio(0, LeaseInfo(-lease.amount, 0), Map.empty))))
      } else Left(TransactionValidationError(tx, s"LeaseTransaction was leased by other sender " +
        s"and time=$time > allowMultipleLeaseCancelTransactionUntilTimestamp=${settings.allowMultipleLeaseCancelTransactionUntilTimestamp}"))

    } yield Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(lease.id -> false))
  }
}

