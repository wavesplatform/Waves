package scorex.api.http

import com.wavesplatform.state2.ByteStr
import scorex.transaction.ValidationError

trait BroadcastRequest {
  protected def parseBase58(v: String, error: String, maxLength: Int): Either[ValidationError, ByteStr] =
    if (v.length > maxLength) Left(ValidationError.GenericError(error))
    else ByteStr.decodeBase58(v).toOption.toRight(ValidationError.GenericError(error))

  protected def parseBase58(v: Option[String], error: String, maxLength: Int): Either[ValidationError, ByteStr] =
    v.fold[Either[ValidationError, ByteStr]](Right(ByteStr(Array.emptyByteArray)))(_v => parseBase58(_v, error, maxLength))

  protected def parseBase58ToOption(v: Option[String], error: String, maxLength: Int): Either[ValidationError, Option[ByteStr]] =
    v.fold[Either[ValidationError, Option[ByteStr]]](Right(None)) { s => parseBase58(s, error, maxLength).map(b => Option(b)) }
}
