package scorex.account

import java.nio.charset.Charset

import scorex.serialization.BytesSerializable
import scorex.transaction.ValidationError

sealed trait Alias extends AccountOrAlias {
  lazy val stringRepr: String = name
  lazy val bytes: Array[Byte] = Alias.AddressVersion +: BytesSerializable.arrayWithSize(name.getBytes(Charset.forName("UTF-8")))

  val name: String
}

object Alias {

  val AddressVersion : Byte = 2

  private case class AliasImpl(name: String) extends Alias

  def apply(name: String): Alias = AliasImpl(name)

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {
    bytes.headOption match {
      case Some(AddressVersion) => Right(AliasImpl(new String(bytes.drop(3), Charset.forName("UTF-8"))))
      case _ => Left(ValidationError.InvalidAddress)
    }
  }
}
