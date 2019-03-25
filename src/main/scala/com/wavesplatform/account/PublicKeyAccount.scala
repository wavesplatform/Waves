package com.wavesplatform.account

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.ValidationError.InvalidAddress
import com.wavesplatform.utils.base58Length

trait PublicKeyAccount {
  def publicKey: Array[Byte]

  lazy val toAddress: Address = Address.fromPublicKey(publicKey)

  override def equals(obj: Any): Boolean = obj match {
    case a: PublicKeyAccount => java.util.Arrays.equals(this.publicKey, a.publicKey)
    case _                   => false
  }

  override def hashCode(): Int       = java.util.Arrays.hashCode(publicKey)
  override lazy val toString: String = this.toAddress.address
}

object PublicKeyAccount {
  val KeyStringLength: Int = base58Length(KeyLength)

  val empty = apply(Array.emptyByteArray)

  def apply(publicKey: Array[Byte]): PublicKeyAccount = {
    final case class PublicKeyAccountImpl(publicKey: Array[Byte]) extends PublicKeyAccount
    PublicKeyAccountImpl(publicKey)
  }

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address =
    publicKeyAccount.toAddress

  def fromBase58String(base58: String): Either[InvalidAddress, PublicKeyAccount] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKeyAccount(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))
}
