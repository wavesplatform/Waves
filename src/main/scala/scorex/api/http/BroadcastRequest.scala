package scorex.api.http

import com.wavesplatform.state2.{ByteArray, EqByteArray}
import scorex.transaction.ValidationError

trait BroadcastRequest {
  protected def parseBase58(v: String, error: String, maxLength: Int): Either[ValidationError, ByteArray] =
    if (v.length > maxLength) Left(ValidationError.TransactionParameterValidationError(error))
    else EqByteArray.decode(v).toOption.toRight(ValidationError.TransactionParameterValidationError(error))

  protected def parseBase58(v: Option[String], error: String, maxLength: Int): Either[ValidationError, ByteArray] =
    v.fold[Either[ValidationError, ByteArray]](Right(EqByteArray(Array.emptyByteArray)))(_v => parseBase58(_v, error, maxLength))

  protected def parseBase58ToOption(v: Option[String], error: String, maxLength: Int): Either[ValidationError, Option[ByteArray]] =
    v.fold[Either[ValidationError, Option[ByteArray]]](Right(None)) { s => parseBase58(s, error, maxLength).map(b => Option(b)) }
}
