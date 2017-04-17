package scorex.account

import com.wavesplatform.utils.base58Length
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash._
import scorex.utils.ScorexLogging

@SerialVersionUID(-5326597598126993189L)
class Account(val address: String) extends Serializable {

  lazy val bytes = Base58.decode(address).get

  override def toString: String = address

  override def equals(b: Any): Boolean = b match {
    case a: Account => a.address == address
    case _ => false
  }

  override def hashCode(): Int = address.hashCode()
}


object Account extends ScorexLogging {

  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val HashLength = 20
  val AddressLength = 1 + 1 + ChecksumLength + HashLength
  val AddressStringLength = base58Length(AddressLength)

  private def scheme = AddressScheme.current

  /**
   * Create account from public key.
   */
  def fromPublicKey(publicKey: Array[Byte]): Account = {
    new Account(addressFromPublicKey(publicKey))
  }

  def addressFromPublicKey(publicKey: Array[Byte]) : String = {
    val publicKeyHash = hash(publicKey).take(HashLength)
    val withoutChecksum = AddressVersion +: scheme.chainId +: publicKeyHash
    Base58.encode(withoutChecksum ++ calcCheckSum(withoutChecksum))
  }

  def isValid(account: Account): Boolean = isValidAddress(account.address)

  def isValidAddress(address: String): Boolean = (address.length <= AddressStringLength) &&
    Base58.decode(address).map { addressBytes =>
      val version = addressBytes.head
      val network = addressBytes.tail.head
      if (version != AddressVersion) {
        log.warn(s"Unknown address version: $version")
        false
      } else if (network != scheme.chainId) {
        log.warn(s"~ For address: $address")
        log.warn(s"~ Expected network: ${scheme.chainId}(${scheme.chainId.toChar}")
        log.warn(s"~ Actual network: $network(${network.toChar}")
        false
      } else {
        if (addressBytes.length != Account.AddressLength)
          false
        else {
          val checkSum = addressBytes.takeRight(ChecksumLength)

          val checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))

          checkSum.sameElements(checkSumGenerated)

        }
      }
    }.getOrElse(false)

  private def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = hash(withoutChecksum).take(ChecksumLength)

}
