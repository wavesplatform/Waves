package scorex.account

import scorex.serialization.BytesSerializable
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.TransactionParameterValidationError
import scorex.account.Alias._

sealed trait Alias extends AccountOrAlias {
  lazy val stringRepr: String = name
  lazy val bytes: Array[Byte] = Alias.AddressVersion +: schemeByte +: BytesSerializable.arrayWithSize(name.getBytes("UTF-8"))

  val name: String
}

object Alias {

  private def schemeByte: Byte = AddressScheme.current.chainId

  val AddressVersion: Byte = 2

  val MinLength = 4
  val MaxLength = 30

  private case class AliasImpl(name: String) extends Alias

  def apply(name: String): Either[ValidationError, Alias] = if (MinLength to MaxLength contains name.length)
    Right(AliasImpl(name))
  else Left(TransactionParameterValidationError(s"Alias '$name' length should be between $MinLength and $MaxLength"))

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {
    bytes.headOption match {
      case Some(AddressVersion) =>
        if (bytes.tail.head == schemeByte)
          Right(AliasImpl(new String(bytes.drop(4), "UTF-8"))) else
          Left(TransactionParameterValidationError("Alias network byte doesn't match current scheme"))
      case _ => Left(TransactionParameterValidationError("Bad alias bytes"))
    }
  }
}
