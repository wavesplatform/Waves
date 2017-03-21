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

  private val ExcetionTxIds = Seq.empty[ByteArray]

  def apply(stateReader: StateReader, settings: FunctionalitySettings, height: Int)(tx: PaymentTransaction): Either[StateValidationError, Diff] = {

    val maybePtxId = stateReader.paymentTransactionIdByHash(EqByteArray(tx.hash))
    if (maybePtxId.exists(txId => !ExcetionTxIds.contains(txId)))
      Left(TransactionValidationError(tx, s"PaymentTx hash already registered and is not an exception"))
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