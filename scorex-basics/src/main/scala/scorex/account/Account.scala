package scorex.account

import scorex.crypto.{Base58, RIPEMD160}


class Account(val address: String) extends Serializable {

  lazy val bytes = Base58.decode(address).get

  override def toString = address

  override def equals(b: Any) = b match {
    case a: Account => a.address == address
    case _ => false
  }

  override def hashCode(): Int = address.hashCode()
}


object Account {

  import scorex.crypto.Sha256._

  val AddressLength = 25

  val AddressVersion: Byte = 58
  val ChecksumLength = 4
  
  def fromBytes(bytes: Array[Byte]) = new Account(Base58.encode(bytes))

  def fromPubkey(publicKey: Array[Byte]) = {
    val publicKeyHash = new RIPEMD160().digest(hash(publicKey))
    val withoutChecksum = publicKeyHash :+ AddressVersion //prepend ADDRESS_VERSION
    val checkSum = doubleHash(withoutChecksum).take(ChecksumLength)

    Base58.encode(withoutChecksum ++ checkSum)
  }

  def isValidAddress(address: String): Boolean =
    Base58.decode(address).map { addressBytes =>
      if (addressBytes.length != Account.AddressLength)
        false
      else {
        val checkSum = addressBytes.takeRight(ChecksumLength)

        val dh = doubleHash(addressBytes.dropRight(ChecksumLength))
        val checkSumGenerated = dh.take(ChecksumLength)

        checkSum.sameElements(checkSumGenerated)
      }
    }.getOrElse(false)
}