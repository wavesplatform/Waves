package scorex.account

import scorex.transaction.ValidationError

sealed trait Alias extends AccountOrAlias {
  lazy val stringRepr: String = name
  lazy val bytes: Array[Byte] = Array.fill(Account.AddressLength)(0)

  val name: String
}

object Alias {

  private case class AliasImpl(name: String) extends Alias

  def apply(name: String): Alias = AliasImpl(name)

  def parseBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = ???
}
