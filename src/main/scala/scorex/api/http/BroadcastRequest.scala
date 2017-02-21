package scorex.api.http

import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError

trait BroadcastRequest {
  protected def parseBase58(v: String, error: String, maxLength: Int): Either[ValidationError, Array[Byte]] =
    if (v.length > maxLength) Left(ValidationError.CustomValidationError(error))
    else Base58.decode(v).toOption.toRight(ValidationError.CustomValidationError(error))

  protected def parseBase58(v: Option[String], error: String, maxLength: Int): Either[ValidationError, Array[Byte]] =
    v.fold[Either[ValidationError, Array[Byte]]](Right(Array.emptyByteArray))(_v => parseBase58(_v, error, maxLength))

  protected def parseBase58ToOption(v: Option[String], error: String, maxLength: Int): Either[ValidationError, Option[Array[Byte]]] =
    v.fold[Either[ValidationError, Option[Array[Byte]]]](Right(None)) { s => parseBase58(s, error, maxLength).map(b => Option(b)) }
}
