package com.wavesplatform.account

import com.google.common.collect.Interners
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.*
import com.wavesplatform.transaction.TxValidationError.InvalidAddress
import com.wavesplatform.utils.base58Length
import org.web3j.crypto.Keys
import play.api.libs.json.{Format, Writes}

opaque type PublicKey = ByteStr

object PublicKey {
  private val interner = Interners.newWeakInterner[PublicKey]()

  private val KeyStringLength: Int = base58Length(KeyLength)

  def isValidSize(length: Int): Boolean = length == KeyLength || length == EthereumKeyLength

  def apply(publicKey: ByteStr): PublicKey = {
    require(isValidSize(publicKey.size), s"invalid public key length: ${publicKey.arr.length}")
    interner.intern(publicKey)
  }

  def apply(publicKey: Array[Byte]): PublicKey =
    apply(ByteStr(publicKey))

  def fromBase58String(base58: String): Either[InvalidAddress, PublicKey] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKey(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))

  def unapply(arg: Array[Byte]): Option[PublicKey] =
    Some(apply(arg))

  extension (pk: PublicKey) {
    def arr: Array[Byte]   = pk.arr
    def byteStr: ByteStr   = pk
    def toAddress: Address = toAddress(AddressScheme.current.chainId)
    def toAddress(chainId: Byte): Address = pk.size match {
      case KeyLength         => Address.fromPublicKey(pk, chainId)
      case EthereumKeyLength => Address(Keys.getAddress(pk.arr), chainId)
      case other             => throw new IllegalArgumentException(s"Unexpected public key length: $other")
    }
  }

  given Format[PublicKey] = Format[PublicKey](
    com.wavesplatform.utils.byteStrFormat.map(this.apply),
    Writes(pk => com.wavesplatform.utils.byteStrFormat.writes(pk))
  )
}
