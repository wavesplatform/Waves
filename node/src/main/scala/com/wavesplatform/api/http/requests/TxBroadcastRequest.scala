package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.validation.TxValidator

trait TxBroadcastRequest[TransactionT <: Transaction] {
  def sender: Option[String]
  def senderPublicKey: Option[String]

  def toTxFrom(sender: PublicKey): Either[ValidationError, TransactionT]

  def toTx: Either[ValidationError, TransactionT] =
    for {
      sender <- senderPublicKey match {
        case Some(key) => PublicKey.fromBase58String(key)
        case None      => Left(GenericError("invalid.senderPublicKey"))
      }
      tx <- toTxFrom(sender)
    } yield tx

  def toValidTxFrom(sender: PublicKey)(implicit v: TxValidator[TransactionT]): Either[ValidationError, TransactionT] =
    for {
      tx <- toTxFrom(sender)
      _  <- tx.validatedEither
    } yield tx

  def toValidTx(implicit v: TxValidator[TransactionT]): Either[ValidationError, TransactionT] =
    for {
      tx <- toTx
      _  <- tx.validatedEither
    } yield tx
}
