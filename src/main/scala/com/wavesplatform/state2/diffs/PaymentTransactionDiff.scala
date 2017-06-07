package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.account.Account
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{PaymentTransaction, ValidationError}

import scala.util.{Left, Right}

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, settings: FunctionalitySettings, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {

    stateReader.paymentTransactionIdByHash(ByteStr(tx.hash)) match {
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
          )),
      paymentTransactionIdsByHashes = Map(ByteStr(tx.hash) -> tx.id)
      ))
    }
  }
}