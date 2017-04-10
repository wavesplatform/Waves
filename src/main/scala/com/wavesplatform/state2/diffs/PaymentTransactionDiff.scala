package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteArray, Diff, EqByteArray, LeaseInfo, Portfolio}
import scorex.account.Account
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{PaymentTransaction, StateValidationError}

import scala.util.{Left, Right}

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[StateValidationError, Diff] = {

    stateReader.paymentTransactionIdByHash(EqByteArray(tx.hash)) match {
      case Some(existing) if blockTime >= settings.requirePaymentUniqueId => Left(TransactionValidationError(tx, s"PaymentTx is already registered: $existing"))
      case _ => Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.recipient -> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty)) combine Map(
          Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
            balance = -tx.amount - tx.fee,
            LeaseInfo.empty,
            assets = Map.empty
          ))))
    }
  }
}