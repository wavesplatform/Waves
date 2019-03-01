package com.wavesplatform.transaction

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.protobuf.Transaction.Data
import com.wavesplatform.transaction.protobuf.{toAmount, SignedTransaction => PBSignedTransaction}
import com.wavesplatform.transaction.transfer.TransferTransactionV2

object PBTransactionFactory {
  def create(signedTx: PBSignedTransaction): Either[ValidationError, Transaction] = {
    for {
      parsedTx <- signedTx.transaction.toRight(GenericError("Transaction must be specified"))
      fee      <- parsedTx.fee.toRight(GenericError("Fee must be specified"))
      _        <- Either.cond(parsedTx.data.isDefined, (), GenericError("Transaction data must be specified"))
      (feeAmount, feeAssetId) = toAmount(fee)
      tx <- create(
        parsedTx.version,
        PublicKeyAccount(parsedTx.senderPublicKey.toByteArray),
        feeAmount,
        feeAssetId,
        parsedTx.timestamp,
        Proofs(signedTx.proofs.map(bs => ByteStr(bs.toByteArray))),
        parsedTx.data
      )
    } yield tx
  }

  def create(version: Int,
             sender: PublicKeyAccount,
             feeAmount: Long,
             feeAssetId: Option[ByteStr],
             timestamp: Long,
             proofs: Proofs,
             data: Data): Either[ValidationError, Transaction] =
    data match {
      case td: Data.Transfer =>
        version match {
          case 2 => TransferTransactionV2.create(sender, timestamp, feeAssetId, feeAmount, proofs, td.value)
        }
    }
}
