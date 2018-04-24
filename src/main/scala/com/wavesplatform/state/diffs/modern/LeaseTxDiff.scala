package com.wavesplatform.state.diffs.modern

import cats.Monoid
import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import scorex.account.Address
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.GenericError
import scorex.transaction.modern.lease.{LeaseCancelTx, LeaseTx}

import scala.util.{Left, Right}

object LeaseTxDiff {

  def lease(blockchain: Blockchain, height: Int)(tx: LeaseTx): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.header.sender.publicKey)
    blockchain.resolveAliasEi(tx.payload.recipient).flatMap { recipient =>
      if (recipient == sender)
        Left(GenericError("Cannot lease to self"))
      else {
        val ap = blockchain.portfolio(tx.sender)
        if (ap.balance - ap.lease.out < tx.payload.amount) {
          Left(GenericError(s"Cannot lease more than own: Balance:${ap.balance}, already leased: ${ap.lease.out}"))
        } else {
          val portfolioDiff: Map[Address, Portfolio] = Map(
            sender    -> Portfolio(-tx.header.fee, LeaseBalance(0, tx.payload.amount), Map.empty),
            recipient -> Portfolio(0, LeaseBalance(tx.payload.amount, 0), Map.empty)
          )
          Right(Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(tx.id() -> true)))
        }
      }
    }
  }

  def leaseCancel(blockchain: Blockchain, settings: FunctionalitySettings, time: Long, height: Int)(
      tx: LeaseCancelTx): Either[ValidationError, Diff] = {
    val leaseEi = blockchain.leaseDetails(tx.payload.leaseId) match {
      case None    => Left(GenericError(s"Related LeaseTransaction not found"))
      case Some(l) => Right(l)
    }
    for {
      lease     <- leaseEi
      recipient <- blockchain.resolveAliasEi(lease.recipient)
      isLeaseActive = lease.isActive
      _ <- if (!isLeaseActive && time > settings.allowMultipleLeaseCancelTransactionUntilTimestamp)
        Left(GenericError(s"Cannot cancel already cancelled lease"))
      else Right(())
      canceller = Address.fromPublicKey(tx.sender.publicKey)
      portfolioDiff <- if (tx.sender == lease.sender) {
        Right(
          Monoid.combine(
            Map(canceller -> Portfolio(-tx.header.fee, LeaseBalance(0, -lease.amount), Map.empty)),
            Map(recipient -> Portfolio(0, LeaseBalance(-lease.amount, 0), Map.empty))
          ))
      } else if (time < settings.allowMultipleLeaseCancelTransactionUntilTimestamp) { // cancel of another acc
        Right(
          Monoid.combine(
            Map(canceller -> Portfolio(-tx.header.fee, LeaseBalance(0, -lease.amount), Map.empty)),
            Map(recipient -> Portfolio(0, LeaseBalance(-lease.amount, 0), Map.empty))
          ))
      } else
        Left(
          GenericError(
            s"LeaseTransaction was leased by other sender " +
              s"and time=$time > allowMultipleLeaseCancelTransactionUntilTimestamp=${settings.allowMultipleLeaseCancelTransactionUntilTimestamp}"))

    } yield Diff(height = height, tx = tx, portfolios = portfolioDiff, leaseState = Map(tx.payload.leaseId -> false))
  }
}
