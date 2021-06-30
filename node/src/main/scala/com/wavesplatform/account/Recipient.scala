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
import com.wavesplatform.utils.{StringBytes, base58Length}
import org.web3j.utils.Numeric.toHexString
import play.api.libs.json._

sealed trait Recipient {
  def bytes: Array[Byte]
  override def equals(obj: Any): Boolean = obj match {
    case r: Recipient => java.util.Arrays.equals(bytes, r.bytes)
    case _            => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
}

object Recipient {
  def fromString(s: String): Either[ValidationError, Recipient] = ???
}

abstract class Address(override val bytes: Array[Byte]) extends Recipient {
  def publicKeyHash: Array[Byte]
}

class WavesAddress(val chainId: Byte, val publicKeyHash: Array[Byte], checksum: Array[Byte])
    extends Address(Array(1.toByte, chainId) ++ publicKeyHash ++ checksum) {
  override lazy val toString: String = ByteStr(bytes).toString
}

class EthereumAddress(val publicKeyHash: Array[Byte]) extends Address(publicKeyHash) {
  override lazy val toString: String = toHexString(bytes)
}

class Alias(val chainId: Byte, val name: String) extends Recipient {

  override lazy val bytes: Array[Byte] = Bytes.concat(Array(Alias.AddressVersion, chainId), Deser.serializeArrayWithLength(name.utf8Bytes))

  override lazy val toString: String = s"alias:${chainId.toChar}:$name"
}

object AddressOrAlias {
  def fromRide(r: RideRecipient): Either[ValidationError, AddressOrAlias] =
    r match {
      case RideRecipient.Address(bytes) => Address.fromBytes(bytes.arr).map(Left(_))
      case RideRecipient.Alias(name)    => Alias.create(name).map(Right(_))
    }

  def fromString(s: String): Either[ValidationError, AddressOrAlias]       = ???
  def fromBytes(buf: ByteBuffer): Either[ValidationError, AddressOrAlias]  = ???
  def fromBytes(buf: Array[Byte]): Either[ValidationError, AddressOrAlias] = ???
}

object Address {
  val Prefix: String           = "address:"
  val AddressVersion: Byte     = 1
  val ChecksumLength: Int      = 4
  val HashLength: Int          = 20
  val AddressLength: Int       = 1 + 1 + HashLength + ChecksumLength
  val AddressStringLength: Int = base58Length(AddressLength)

  private[this] val publicKeyBytesCache: Cache[(ByteStr, Byte), WavesAddress] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  private[this] val bytesCache: Cache[ByteStr, Either[InvalidAddress, WavesAddress]] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  def fromPublicKey(publicKey: PublicKey, chainId: Byte = scheme.chainId): WavesAddress = {
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

  def fromBytes(addressBytes: Array[Byte], chainId: Byte = scheme.chainId): Either[InvalidAddress, WavesAddress] = {
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
              val Array(version, network, _*) = addressBytes

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

  def fromString(addressStr: String): Either[ValidationError, WavesAddress] = {
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
  private[wavesplatform] def createUnsafe(addressBytes: Array[Byte]): WavesAddress =
    new WavesAddress(addressBytes(1), addressBytes.drop(2).dropRight(4), addressBytes.takeRight(4))
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
