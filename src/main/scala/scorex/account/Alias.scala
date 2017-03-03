package scorex.account

import java.nio.charset.Charset

import scorex.serialization.BytesSerializable
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.TransactionParameterValidationError

sealed trait Alias extends AccountOrAlias {
  lazy val stringRepr: String = name
  lazy val bytes: Array[Byte] = Alias.AddressVersion +: BytesSerializable.arrayWithSize(name.getBytes(Charset.forName("UTF-8")))

  val name: String
}

object Alias {

  val AddressVersion: Byte = 2

  val MinLength = 4
  val MaxLength = 30

  private case class AliasImpl(name: String) extends Alias

  def apply(name: String): Either[ValidationError, Alias] = if (MinLength to MaxLength contains name.length)
    Right(AliasImpl(name))
  else Left(TransactionParameterValidationError(s"Alias '$name' length should be between $MinLength and $MaxLength"))

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {
    bytes.headOption match {
      case Some(AddressVersion) => Right(AliasImpl(new String(bytes.drop(3), Charset.forName("UTF-8"))))
      case _ => Left(TransactionParameterValidationError("Bad alias bytes"))
    }
  }
}
