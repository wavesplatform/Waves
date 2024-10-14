package com.wavesplatform.account

import com.google.common.cache.{Cache, CacheBuilder}

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.Recipient as RideRecipient
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvalidAddress, WrongChain}
import com.wavesplatform.utils.{EthEncoding, StringBytes, base58Length}
import play.api.libs.json.*

sealed trait AddressOrAlias {
  def chainId: Byte
  def bytes: Array[Byte]
}

final class Address private (val chainId: Byte, val publicKeyHash: Array[Byte], checksum: Array[Byte]) extends AddressOrAlias {
  override lazy val bytes: Array[Byte] = Bytes.concat(Array(1.toByte, chainId), publicKeyHash, checksum)
  override lazy val toString: String   = ByteStr(bytes).toString

  override def equals(obj: Any): Boolean = obj match {
    case a: Address => java.util.Arrays.equals(publicKeyHash, a.publicKeyHash) && chainId == a.chainId
    case _          => false
  }

  private lazy val hc = Ints.fromByteArray(checksum)

  override def hashCode(): Int = hc
}

final case class Alias(chainId: Byte, name: String) extends AddressOrAlias {
  override lazy val bytes: Array[Byte] = Bytes.concat(Array(Alias.AddressVersion, chainId), Deser.serializeArrayWithLength(name.utf8Bytes))
  override lazy val toString: String   = s"alias:${chainId.toChar}:$name"
}

object AddressOrAlias {
  def fromRide(r: RideRecipient): Either[ValidationError, AddressOrAlias] =
    r match {
      case RideRecipient.Address(bytes) => Address.fromBytes(bytes.arr, Some(AddressScheme.current.chainId))
      case RideRecipient.Alias(name)    => Alias.create(name)
    }

  def fromString(s: String, expectedChainId: Option[Byte] = None): Either[ValidationError, AddressOrAlias] =
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s, expectedChainId)
    else
      Address.fromString(s, expectedChainId)

  def fromBytes(buf: Array[Byte]): Either[ValidationError, AddressOrAlias] = buf.headOption match {
    case Some(Address.AddressVersion) => Address.fromBytes(buf, None)
    case Some(Alias.AddressVersion)   => Alias.fromBytes(buf, None)
    case _                            => throw new IllegalArgumentException(s"Not a valid recipient: ${ByteStr(buf)}")
  }
}

object Address {
  val Prefix: String           = "address:"
  val AddressVersion: Byte     = 1
  val ChecksumLength: Int      = 4
  val HashLength: Int          = 20
  val AddressLength: Int       = 1 + 1 + HashLength + ChecksumLength
  val AddressStringLength: Int = base58Length(AddressLength)

  private val publicKeyBytesCache: Cache[(ByteStr, Byte), Address] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  private val bytesCache: Cache[ByteStr, Either[InvalidAddress, Address]] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  def apply(publicKeyHash: Array[Byte], chainId: Byte = AddressScheme.current.chainId): Address = {
    require(publicKeyHash.length == HashLength, "Public key hash should be 20 bytes long")
    val checksumPayload = Bytes.concat(Array(1.toByte, AddressScheme.current.chainId), publicKeyHash)
    val checksum        = crypto.secureHash(checksumPayload)
    new Address(chainId, publicKeyHash, checksum.take(4))
  }

  def fromHexString(hexString: String): Address =
    Address(EthEncoding.toBytes(hexString))

  def fromPublicKey(publicKey: PublicKey, chainId: Byte = scheme.chainId): Address = {
    publicKeyBytesCache.get(
      (publicKey, chainId),
      { () =>
        val withoutChecksum = ByteBuffer
          .allocate(1 + 1 + HashLength)
          .put(AddressVersion)
          .put(chainId)
          .put(crypto.secureHash(publicKey.arr), 0, HashLength)
          .array()

        val bytes = ByteBuffer
          .allocate(AddressLength)
          .put(withoutChecksum)
          .put(calcCheckSum(withoutChecksum), 0, ChecksumLength)
          .array()

        createUnsafe(bytes)
      }
    )
  }

  def fromBytes(addressBytes: Array[Byte], expectedChainId: Option[Byte] = Some(AddressScheme.current.chainId)): Either[InvalidAddress, Address] = {
    bytesCache.get(
      ByteStr(addressBytes),
      { () =>
        Either
          .cond(
            addressBytes.length == Address.AddressLength,
            (),
            InvalidAddress(s"Wrong addressBytes length: expected: ${Address.AddressLength}, actual: ${addressBytes.length}")
          )
          .flatMap { _ =>
            val Array(version, network, _*) = addressBytes: @unchecked

            (for {
              _ <- Either.cond(version == AddressVersion, (), s"Unknown address version: $version")
              _ <- expectedChainId match {
                case Some(exp) if exp != network =>
                  Left(s"Address belongs to another network: expected: $exp(${exp.toChar}), actual: $network(${network.toChar})")
                case _ => Right(())
              }
              checkSum          = addressBytes.takeRight(ChecksumLength)
              checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
              _ <- Either.cond(java.util.Arrays.equals(checkSum, checkSumGenerated), (), s"Bad address checksum")
            } yield createUnsafe(addressBytes)).left.map(err => InvalidAddress(err))
          }
      }
    )
  }

  def fromString(addressStr: String, expectedChainId: Option[Byte] = Some(AddressScheme.current.chainId)): Either[ValidationError, Address] = {
    val base58String = if (addressStr.startsWith(Prefix)) addressStr.drop(Prefix.length) else addressStr
    for {
      _ <- Either.cond(
        base58String.length <= AddressStringLength,
        (),
        InvalidAddress(s"Wrong address string length: max=$AddressStringLength, actual: ${base58String.length}")
      )
      byteArray <- Base58.tryDecodeWithLimit(base58String).toEither.left.map(ex => InvalidAddress(s"Unable to decode base58: ${ex.getMessage}"))
      address   <- fromBytes(byteArray, expectedChainId)
    } yield address
  }

  def calcCheckSum(withoutChecksum: Array[Byte]): Array[Byte] = {
    val fullHash = crypto.secureHash(withoutChecksum)
    fullHash.take(ChecksumLength)
  }

  implicit val jsonFormat: Format[Address] = Format[Address](
    Reads(jsValue => fromString(jsValue.as[String]).fold(err => JsError(err.toString), JsSuccess(_))),
    Writes(addr => JsString(addr.toString))
  )

  @inline
  private def scheme: AddressScheme = AddressScheme.current

  // Optimization, should not be used externally
  private def createUnsafe(addressBytes: Array[Byte]): Address =
    new Address(addressBytes(1), addressBytes.drop(2).dropRight(4), addressBytes.takeRight(4))
}

object Alias {
  val Prefix: String = "alias:"

  val AddressVersion: Byte = 2
  val MinLength            = 4
  val MaxLength            = 30

  val AliasAlphabet = "-.0123456789@_abcdefghijklmnopqrstuvwxyz"

  def create(name: String): Either[ValidationError, Alias] = {
    createWithChainId(name, AddressScheme.current.chainId)
  }

  def fromString(str: String, expectedChainId: Option[Byte] = Some(AddressScheme.current.chainId)): Either[ValidationError, Alias] = {
    val aliasPatternInfo = "Alias string pattern is 'alias:<chain-id>:<address-alias>"

    if (!str.startsWith(Prefix)) {
      Left(GenericError(aliasPatternInfo))
    } else {
      val charSemicolonAlias = str.drop(Prefix.length)
      val chainId            = charSemicolonAlias(0).toByte
      val name               = charSemicolonAlias.drop(2)
      if (charSemicolonAlias(1) != ':') {
        Left(GenericError(aliasPatternInfo))
      } else {
        createWithChainId(name, chainId, expectedChainId)
      }
    }
  }

  def fromBytes(bytes: Array[Byte], expectedChainId: Option[Byte]): Either[ValidationError, Alias] = {
    bytes match {
      case Array(`AddressVersion`, chainId, _, _, rest*) =>
        createWithChainId(new String(rest.toArray, "UTF-8"), chainId, expectedChainId)

      case _ =>
        Left(GenericError("Bad alias bytes"))
    }
  }

  private def isValidAliasChar(c: Char): Boolean =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || c == '_' || c == '@' || c == '-' || c == '.'

  private[wavesplatform] def createWithChainId(
      name: String,
      chainId: Byte,
      expectedChainId: Option[Byte] = Some(AddressScheme.current.chainId)
  ): Either[ValidationError, Alias] =
    if (name.length < MinLength || MaxLength < name.length)
      Left(GenericError(s"Alias '$name' length should be between $MinLength and $MaxLength"))
    else if (!name.forall(isValidAliasChar))
      Left(GenericError(s"Alias should contain only following characters: $AliasAlphabet"))
    else
      expectedChainId match {
        case Some(expected) if expected != chainId => Left(WrongChain(expected, chainId))
        case _                                     => Right(Alias(chainId, name))
      }

  implicit val writes: Writes[Alias] = Writes(a => JsString(a.toString))
}
