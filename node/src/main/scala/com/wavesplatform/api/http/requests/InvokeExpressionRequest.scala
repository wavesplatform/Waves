package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.InvokeExpressionTransaction
import play.api.libs.json.*

case class InvokeExpressionRequest(
    version: Option[Byte],
    senderPublicKey: String,
    fee: Long,
    feeAssetId: Option[String],
    expression: String,
    timestamp: Long,
    proofs: Proofs
) extends TxBroadcastRequest[InvokeExpressionTransaction] {
  def toTx: Either[ValidationError, InvokeExpressionTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _feeAssetId <- parseBase58ToAsset(feeAssetId.filter(_.nonEmpty), "Invalid feeAssetId")
      _expression <- Script.fromBase64String(expression).flatMap {
        case script: ExprScript => Right(script)
        case _                  => Left(GenericError("Unexpected expression type for InvokeExpression"))
      }
      t <- InvokeExpressionTransaction.create(
        version.getOrElse(1.toByte),
        _sender,
        _expression,
        fee,
        _feeAssetId,
        timestamp,
        proofs
      )
    } yield t
}

object InvokeExpressionRequest {
  given Reads[InvokeExpressionRequest] = Json.reads
}
