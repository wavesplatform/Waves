package com.wavesplatform.account

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.ValidationError.InvalidAddress
import com.wavesplatform.utils.base58Length

sealed trait PublicKeyAccount {
  def publicKey: Array[Byte]
  def toAddress: Address
}

object PublicKeyAccount {
  val KeyStringLength: Int = base58Length(KeyLength)

  val empty = apply(Array.emptyByteArray)

  def apply(publicKey: Array[Byte]): PublicKeyAccount = {
    final class PublicKeyAccountImpl(val publicKey: Array[Byte]) extends PublicKeyAccount {
      override lazy val toAddress: Address = Address.fromPublicKey(publicKey)
      override lazy val toString: String = this.toAddress.address

      override def equals(obj: Any): Boolean = obj match {
        case a: PublicKeyAccount => java.util.Arrays.equals(this.publicKey, a.publicKey)
        case _                   => false
      }
      override def hashCode(): Int       = java.util.Arrays.hashCode(publicKey)
    }

    new PublicKeyAccountImpl(publicKey)
  }

  def unapply(arg: PublicKeyAccount): Option[Array[Byte]] =
    Some(arg.publicKey)

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address =
    publicKeyAccount.toAddress

  def fromBase58String(base58: String): Either[InvalidAddress, PublicKeyAccount] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKeyAccount(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))
}
