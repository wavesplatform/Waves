package com.wavesplatform.account
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.google.common.cache.{Cache, CacheBuilder}
import com.google.common.primitives.Bytes
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.ChainId
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.utils.{ScorexLogging, base58Length}
import play.api.libs.json._

trait AddressOrAlias {
  def stringRepr: String
  def bytes: ByteStr
  def chainId: ChainId

  override def toString: String = stringRepr

  override def equals(obj: Any): Boolean = obj match {
    case a: AddressOrAlias => bytes == a.bytes
    case _                 => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object AddressOrAlias {
  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (AddressOrAlias, Int)] = {
    bytes(position) match {
      case Address.AddressVersion =>
        val addressEnd   = position + Address.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Address.fromBytes(addressBytes).map((_, addressEnd))
      case Alias.AddressVersion =>
        val (_, aliasEnd) = Deser.parseArrayWithLength(bytes, position + 2)
        Alias.fromBytes(bytes.slice(position, aliasEnd)).map((_, aliasEnd))
      case _ => Left(InvalidAddress("Unknown address/alias version"))
    }
  }

  def fromBytes(buf: ByteBuffer): Either[ValidationError, AddressOrAlias] = {
    buf.get match {
      case Address.AddressVersion =>
        buf.position(buf.position() - 1)
        val addressBytes = new Array[Byte](Address.AddressLength)
        buf.get(addressBytes)
        Address.fromBytes(addressBytes)

      case Alias.AddressVersion =>
        val chainId    = buf.get
        val aliasBytes = Deser.parseArrayWithLength(buf)
        Alias.createWithChainId(new String(aliasBytes, "UTF-8"), chainId)

      case _ =>
        Left(InvalidAddress("Unknown address/alias version"))
    }
  }

  def fromString(s: String): Either[ValidationError, AddressOrAlias] = {
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s)
    else Address.fromString(s)
  }
}

sealed trait Address extends AddressOrAlias {
  lazy val stringRepr: String = bytes.toString
}

//noinspection ScalaDeprecation
object Address extends ScorexLogging {
  val Prefix               = "address:"
  val AddressVersion: Byte = 1
  val ChecksumLength       = 4
  val HashLength           = 20
  val AddressLength        = 1 + 1 + HashLength + ChecksumLength
  val AddressStringLength  = base58Length(AddressLength)

  private[this] val publicKeyBytesCache: Cache[ByteStr, Address] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  private[this] val bytesCache: Cache[ByteStr, Either[InvalidAddress, Address]] = CacheBuilder
    .newBuilder()
    .softValues()
    .maximumSize(200000)
    .build()

  def fromPublicKey(publicKey: PublicKey, chainId: Byte = ChainId.global): Address = {
    publicKeyBytesCache.get(
      publicKey, { () =>
        val withoutChecksum = ByteBuffer
          .allocate(1 + 1 + HashLength)
          .put(AddressVersion)
          .put(chainId)
          .put(crypto.secureHash(publicKey), 0, HashLength)
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

  def fromBytes(addressBytes: ByteStr, chainId: Byte = ChainId.global): Either[InvalidAddress, Address] = {
    bytesCache.get(
      addressBytes, { () =>
        Either
          .cond(
            addressBytes.length == Address.AddressLength,
            (),
            InvalidAddress(s"Wrong addressBytes length: expected: ${Address.AddressLength}, actual: ${addressBytes.length}")
          )
          .right
          .flatMap {
            res =>
              val Array(version, network, _*) = addressBytes.arr

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
    Writes(addr => JsString(addr.stringRepr))
  )

  // Optimization, should not be used externally
  private[wavesplatform] def createUnsafe(address: ByteStr): Address = {
    final case class AddressImpl(bytes: ByteStr) extends Address {
      override def chainId: ChainId = bytes.arr(1)
    }
    AddressImpl(address)
  }
}

sealed trait Alias extends AddressOrAlias {
  lazy val stringRepr: String = Alias.Prefix + chainId.toChar + ":" + name
  lazy val bytes: ByteStr     = Bytes.concat(Array(Alias.AddressVersion, chainId), Deser.serializeArrayWithLength(name.getBytes(StandardCharsets.UTF_8)))

  val name: String
}

object Alias {
  val Prefix: String = "alias:"

  val AddressVersion: Byte = 2
  val MinLength            = 4
  val MaxLength            = 30

  val AliasAlphabet = "-.0123456789@_abcdefghijklmnopqrstuvwxyz"

  def create(name: String): Either[ValidationError, Alias] = {
    createWithChainId(name, ChainId.global)
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

  def createWithChainId(name: String, chainId: Byte): Either[ValidationError, Alias] = {
    final case class AliasImpl(chainId: Byte, name: String) extends Alias

    if (name.length < MinLength || MaxLength < name.length)
      Left(GenericError(s"Alias '$name' length should be between $MinLength and $MaxLength"))
    else if (!name.forall(isValidAliasChar))
      Left(GenericError(s"Alias should contain only following characters: $AliasAlphabet"))
    else
      Right(AliasImpl(chainId, name))
  }
}
