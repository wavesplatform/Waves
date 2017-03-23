package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteArray, Diff, EqByteArray, Portfolio}
import scorex.account.Account
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{PaymentTransaction, StateValidationError}

import scala.util.{Left, Right}

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, settings: FunctionalitySettings, height: Int)(tx: PaymentTransaction): Either[StateValidationError, Diff] = {

    stateReader.paymentTransactionIdByHash(EqByteArray(tx.hash)) match {
      case Some(existing) => Left(TransactionValidationError(tx, s"PaymentTx is already registered: $existing"))
      case None => Right(Diff(height = height,
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
}