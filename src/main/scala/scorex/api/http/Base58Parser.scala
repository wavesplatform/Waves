package scorex.api.http

import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError

/**
  * Created by ilyas on 20-Feb-17.
  */
object Base58Parser {
  def parseBase58(v: String, error: String): Either[ValidationError, Array[Byte]] =
    Base58.decode(v).toEither.left.map(_ => ValidationError.CustomValidationError(error))

  def parseBase58(v: Option[String], error: String): Either[ValidationError, Array[Byte]] =
    v.fold[Either[ValidationError, Array[Byte]]](Right(Array.emptyByteArray))(_v => parseBase58(_v, error))

  def parseBase58ToOption(v: Option[String], error: String): Either[ValidationError, Option[Array[Byte]]] =
    v.fold[Either[ValidationError, Option[Array[Byte]]]](Right(None)) { s => parseBase58(s, error).map(b => Option(b)) }

}
