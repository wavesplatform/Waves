package com.wavesplatform.state.diffs

import cats._
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.lease._

import scala.util.{Left, Right}

object LeaseTransactionsDiff {

  def lease(blockchain: Blockchain, height: Int)(tx: LeaseTransaction): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender.publicKey)
    blockchain.resolveAlias(tx.recipient).flatMap { recipient =>
      if (recipient == sender)
        Left(GenericError("Cannot lease to self"))
      else {
        val lease   = blockchain.leaseBalance(tx.sender)
        val balance = blockchain.balance(tx.sender, Waves)
        if (balance - lease.out < tx.amount) {
          Left(GenericError(s"Cannot lease more than own: Balance:${balance}, already leased: ${lease.out}"))
        } else {
          val portfolioDiff: Map[Address, Portfolio] = Map(
            sender    -> Portfolio(-tx.fee, LeaseBalance(0, tx.amount), Map.empty),
            recipient -> Portfolio(0, LeaseBalance(tx.amount, 0), Map.empty)
          )
          Right(Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(tx.id() -> true)))
        }
      }
    }
  }

  def leaseCancel(blockchain: Blockchain, settings: FunctionalitySettings, time: Long, height: Int)(
      tx: LeaseCancelTransaction): Either[ValidationError, Diff] = {
    val leaseEi = blockchain.leaseDetails(tx.leaseId) match {
      case None    => Left(GenericError(s"Related LeaseTransaction not found"))
      case Some(l) => Right(l)
    }
    for {
      lease     <- leaseEi
      recipient <- blockchain.resolveAlias(lease.recipient)
      isLeaseActive = lease.isActive
      _ <- if (!isLeaseActive && time > settings.allowMultipleLeaseCancelTransactionUntilTimestamp)
        Left(GenericError(s"Cannot cancel already cancelled lease"))
      else Right(())
      canceller = Address.fromPublicKey(tx.sender.publicKey)
      portfolioDiff <- if (tx.sender == lease.sender) {
        Right(
          Monoid.combine(Map(canceller -> Portfolio(-tx.fee, LeaseBalance(0, -lease.amount), Map.empty)),
                         Map(recipient -> Portfolio(0, LeaseBalance(-lease.amount, 0), Map.empty))))
      } else if (time < settings.allowMultipleLeaseCancelTransactionUntilTimestamp) { // cancel of another acc
        Right(
          Monoid.combine(Map(canceller -> Portfolio(-tx.fee, LeaseBalance(0, -lease.amount), Map.empty)),
                         Map(recipient -> Portfolio(0, LeaseBalance(-lease.amount, 0), Map.empty))))
      } else
        Left(
          GenericError(
            s"LeaseTransaction was leased by other sender " +
              s"and time=$time > allowMultipleLeaseCancelTransactionUntilTimestamp=${settings.allowMultipleLeaseCancelTransactionUntilTimestamp}"))

    } yield Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(tx.leaseId -> false))
  }
}
