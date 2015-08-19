package scorex.account


class Account(val address: String) extends Serializable {

  override def toString = address

  override def equals(b: Any) = b match {
    case a: Account => a.address == address
    case _ => false
  }

  override def hashCode(): Int = address.hashCode()
}


object Account {
  val AddressLength = 25
}