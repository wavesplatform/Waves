package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import cats.Monoid

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{Diff, Portfolio, StateReader}
import scorex.account.Account
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{PaymentTransaction, StateValidationError}

import scala.util.{Left, Right}

object PaymentTransactionDiff {
  def apply(stateReader: StateReader, settings: FunctionalitySettings, height: Int)(tx: PaymentTransaction): Either[StateValidationError, Diff] = {

    lazy val isTimestampCorrect: Boolean = {
      val maybePreviousPmtTx: Option[PaymentTransaction] = stateReader.lastAccountPaymentTransaction(tx.sender)
      maybePreviousPmtTx match {
        case Some(lastTransaction) => lastTransaction.timestamp < tx.timestamp
        case None => true
      }
    }

    val isCorrect = tx.timestamp < settings.allowInvalidPaymentTransactionsByTimestamp || isTimestampCorrect
    if (!isCorrect)
      Left(TransactionValidationError(tx, s" is earlier than previous transaction after time=${settings.allowInvalidPaymentTransactionsByTimestamp}"))
    else Right(Diff(height = height,
      tx = tx,
      portfolios = Map(
        tx.recipient -> Portfolio(
          balance = tx.amount,
          effectiveBalance = tx.amount,
          assets = Map.empty)) combine Map(
        Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
          balance = -tx.amount - tx.fee,
          effectiveBalance = -tx.amount - tx.fee,
          assets = Map.empty
        )
      ),
      assetInfos = Map.empty
    ))
  }
}