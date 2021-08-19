package com.wavesplatform.account

import java.nio.ByteBuffer

import com.google.common.cache.{Cache, CacheBuilder}
import com.google.common.primitives.Bytes
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.{Recipient => RideRecipient}
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvalidAddress}
import com.wavesplatform.utils.{base58Length, EthEncoding, StringBytes}
import play.api.libs.json._

sealed trait AddressOrAlias {
  def chainId: Byte
  def bytes: Array[Byte]
}

final class Address private(val chainId: Byte, val publicKeyHash: Array[Byte], checksum: Array[Byte]) extends AddressOrAlias {
  override lazy val bytes: Array[Byte] = Array(1.toByte, chainId) ++ publicKeyHash ++ checksum
  override lazy val toString: String   = ByteStr(bytes).toString

  override def equals(obj: Any): Boolean = obj match {
    case a: Address => java.util.Arrays.equals(publicKeyHash, a.publicKeyHash) && chainId == a.chainId
    case _          => false
  }
  
  override def hashCode(): Int = (publicKeyHash, chainId).hashCode()
}

final case class Alias(chainId: Byte, name: String) extends AddressOrAlias {
  override lazy val bytes: Array[Byte] = Bytes.concat(Array(Alias.AddressVersion, chainId), Deser.serializeArrayWithLength(name.utf8Bytes))
  override lazy val toString: String   = s"alias:${chainId.toChar}:$name"
}

object AddressOrAlias {
  def fromRide(r: RideRecipient): Either[ValidationError, AddressOrAlias] =
    r match {
      case RideRecipient.Address(bytes) => Address.fromBytes(bytes.arr)
      case RideRecipient.Alias(name)    => Alias.create(name)
    }

  def fromString(s: String): Either[ValidationError, AddressOrAlias] = s match {
    case alias if alias.startsWith(Alias.Prefix) => Alias.fromString(s)
    case address                                 => Address.fromString(address)
  }

  def fromBytes(buf: Array[Byte]): Either[ValidationError, AddressOrAlias] = buf.headOption match {
    case Some(Address.AddressVersion) => Address.fromBytes(buf)
    case Some(Alias.AddressVersion)   => Alias.fromBytes(buf)
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

  private[this] val publicKeyBytesCache: Cache[(ByteStr, Byte), Address] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  private[this] val bytesCache: Cache[ByteStr, Either[InvalidAddress, Address]] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  def apply(publicKeyHash: Array[Byte], chainId: Byte = AddressScheme.current.chainId): Address = new Address(
    chainId,
    publicKeyHash,
    crypto.secureHash(Array(1.toByte, AddressScheme.current.chainId) ++ publicKeyHash).take(4)
  )

  def fromHexString(hexString: String): Address = Address(EthEncoding.toBytes(hexString))

  def fromPublicKey(publicKey: PublicKey, chainId: Byte = scheme.chainId): Address = {
    publicKeyBytesCache.get(
      (publicKey, chainId), { () =>
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

  def fromBytes(addressBytes: Array[Byte], chainId: Byte = scheme.chainId): Either[InvalidAddress, Address] = {
    bytesCache.get(
      ByteStr(addressBytes), { () =>
        Either
          .cond(
            addressBytes.length == Address.AddressLength,
            (),
            InvalidAddress(s"Wrong addressBytes length: expected: ${Address.AddressLength}, actual: ${addressBytes.length}")
          )
          .flatMap {
            _ =>
              val Array(version, network, _*) = (addressBytes: @unchecked)

              (for {
                _ <- Either.cond(version == AddressVersion, (), s"Unknown address version: $version")
                _ <- Either.cond(
                  network == chainId,
                  (),
                  s"Data from other network: expected: $chainId(${chainId.toChar}), actual: $network(${network.toChar})"
                )
                checkSum          = addressBytes.takeRight(ChecksumLength)
                checkSumGenerated = calcCheckSum(addressBytes.dropRight(ChecksumLength))
                _ <- Either.cond(java.util.Arrays.equals(checkSum, checkSumGenerated), (), s"Bad address checksum")
              } yield createUnsafe(addressBytes)).left.map(err => InvalidAddress(err))
          }
      }
    )
  }

  def fromString(addressStr: String): Either[ValidationError, Address] = {
    val base58String = if (addressStr.startsWith(Prefix)) addressStr.drop(Prefix.length) else addressStr
    for {
      _ <- Either.cond(
        base58String.length <= AddressStringLength,
        (),
        InvalidAddress(s"Wrong address string length: max=$AddressStringLength, actual: ${base58String.length}")
      )
      byteArray <- Base58.tryDecodeWithLimit(base58String).toEither.left.map(ex => InvalidAddress(s"Unable to decode base58: ${ex.getMessage}"))
      address   <- fromBytes(byteArray)
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
  private[this] def scheme: AddressScheme = AddressScheme.current

  // Optimization, should not be used externally
  private[this] def createUnsafe(addressBytes: Array[Byte]): Address =
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

  def fromString(str: String): Either[ValidationError, Alias] = {
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
        createWithChainId(name, chainId)
      }
    }
  }

  def fromBytes(bytes: Array[Byte]): Either[ValidationError, Alias] = {
    bytes match {
      case Array(`AddressVersion`, chainId, _, _, rest @ _*) =>
        createWithChainId(new String(rest.toArray, "UTF-8"), chainId)

      case _ =>
        Left(GenericError("Bad alias bytes"))
    }
  }

  private[this] def isValidAliasChar(c: Char): Boolean =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || c == '_' || c == '@' || c == '-' || c == '.'

  private[wavesplatform] def createWithChainId(name: String, chainId: Byte): Either[ValidationError, Alias] = {
    if (name.length < MinLength || MaxLength < name.length)
      Left(GenericError(s"Alias '$name' length should be between $MinLength and $MaxLength"))
    else if (!name.forall(isValidAliasChar))
      Left(GenericError(s"Alias should contain only following characters: $AliasAlphabet"))
    else
      Right(new Alias(chainId, name))
  }
}
