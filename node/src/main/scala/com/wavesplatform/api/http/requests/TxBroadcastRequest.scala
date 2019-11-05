package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError

trait TxBroadcastRequest {
  def sender: Option[String]
  def senderPublicKey: Option[String]

  def toTxFrom(sender: PublicKey): Either[ValidationError, Transaction]

  def toTx: Either[ValidationError, Transaction] =
    for {
      sender <- senderPublicKey match {
        case Some(key) => PublicKey.fromBase58String(key)
        case None      => Left(GenericError("invalid.senderPublicKey"))
      }
      tx <- toTxFrom(sender)
    } yield tx
}
