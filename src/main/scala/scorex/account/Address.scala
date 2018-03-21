package scorex.account

import com.wavesplatform.crypto
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.utils.base58Length
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidAddress
import scorex.utils.ScorexLogging


sealed trait Address extends AddressOrAlias {
  val bytes: ByteStr
  lazy val address: String = bytes.base58
  lazy val stringRepr: String = address

}

object Address extends ScorexLogging {

  val Prefix: String = "address:"

  val AddressVersion: Byte = 1
  val ChecksumLength = 4
  val HashLength = 20
  val AddressLength = 1 + 1 + ChecksumLength + HashLength
  val AddressStringLength = base58Length(AddressLength)

  private def scheme = AddressScheme.current

  private class AddressImpl(val bytes: ByteStr) extends Address

  def fromPublicKey(publicKey: Array[Byte]): Address = {
    val publicKeyHash = crypto.secureHash(publicKey).take(HashLength)
    val withoutChecksum = AddressVersion +: scheme.chainId +: publicKeyHash
    val bytes = withoutChecksum ++ calcCheckSum(withoutChecksum)
    new AddressImpl(ByteStr(bytes))
  }

  def fromBytes(addressBytes: Array[Byte]): Either[InvalidAddress, Address] = {
    val version = addressBytes.head
    val network = addressBytes.tail.head
    (for {
      _ <- Either.cond(version == AddressVersion, (), s"Unknown address version: $version")
      _ <- Either.cond(network == scheme.chainId, (), s"Data from other network: expected: ${scheme.chainId}(${scheme.chainId.toChar}), actual: $network(${network.toChar})")
      _ <- Either.cond(addressBytes.length == Address.AddressLength, (), s"Wrong addressBytes length: expected: ${Address.AddressLength}, actual: ${addressBytes.length}")
      checkSum = addressBytes.takeRight(ChecksumLength)
      checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
      _ <- Either.cond(checkSum.sameElements(checkSumGenerated), (), s"Bad address checksum")
    } yield new AddressImpl(ByteStr(addressBytes))).left.map(InvalidAddress)
  }

  def fromString(addressStr: String): Either[ValidationError, Address] = {
    val base58String = if (addressStr.startsWith(Prefix)) addressStr.drop(Prefix.length) else addressStr
    for {
      _ <- Either.cond(base58String.length <= AddressStringLength, (), InvalidAddress(s"Wrong address string length: max=$AddressStringLength, actual: address.length"))
      byteArray <- Base58.decode(base58String).toEither.left.map(ex => InvalidAddress(s"Unable to decode base58: ${ex.getMessage}"))
      address <- fromBytes(byteArray)
    } yield address
  }

  private def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = crypto.secureHash(withoutChecksum).take(ChecksumLength)

}
