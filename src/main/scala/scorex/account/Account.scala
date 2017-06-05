package scorex.account

import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import scorex.crypto.encode.Base58
import scorex.crypto.hash.SecureCryptographicHash._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidAddress
import scorex.utils.ScorexLogging

import scala.util.Success


sealed trait Account extends AccountOrAlias {
  val bytes: ByteStr
  lazy val address: String = bytes.base58
  lazy val stringRepr: String = Account.Prefix + address

}

object Account extends ScorexLogging {

  val Prefix: String = "address:"

  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val HashLength = 20
  val AddressLength = 1 + 1 + ChecksumLength + HashLength
  val AddressStringLength = base58Length(AddressLength)

  private def scheme = AddressScheme.current

  private class AccountImpl(val bytes: ByteStr) extends Account

  def fromPublicKey(publicKey: Array[Byte]): Account = {
    val publicKeyHash = hash(publicKey).take(HashLength)
    val withoutChecksum = AddressVersion +: scheme.chainId +: publicKeyHash
    val bytes = withoutChecksum ++ calcCheckSum(withoutChecksum)
    new AccountImpl(ByteStr(bytes))
  }

  def fromBytes(addressBytes: Array[Byte]): Either[ValidationError, Account] = {
    if (isByteArrayValid(addressBytes)) Right(new AccountImpl(ByteStr(addressBytes)))
    else Left(InvalidAddress)
  }

  private def fromBase58String(address: String): Either[ValidationError, Account] =
    if (address.length > AddressStringLength) {
      Left(InvalidAddress)
    } else {
      Base58.decode(address) match {
        case Success(byteArray) if isByteArrayValid(byteArray) => Right(new AccountImpl(ByteStr(byteArray)))
        case _ => Left(InvalidAddress)
      }
    }

  def fromString(address: String): Either[ValidationError, Account] = {
    val base58String = if (address.startsWith(Prefix))
      address.drop(Prefix.length)
    else address
    fromBase58String(base58String)
  }


  private def isByteArrayValid(addressBytes: Array[Byte]): Boolean = {
    val version = addressBytes.head
    val network = addressBytes.tail.head
    if (version != AddressVersion) {
      log.warn(s"Unknown address version: $version")
      false
    } else if (network != scheme.chainId) {
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
  }

  private def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = hash(withoutChecksum).take(ChecksumLength)

}
