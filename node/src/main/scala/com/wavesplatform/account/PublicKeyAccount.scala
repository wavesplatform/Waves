package com.wavesplatform.account

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.ValidationError.InvalidAddress
import com.wavesplatform.utils.base58Length

trait PublicKeyAccount {
  def publicKey: Array[Byte]

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.sameElements(a.publicKey)
    case _                   => false
  }

  override def hashCode(): Int = publicKey.hashCode()

  override lazy val toString: String = this.toAddress.address
}

object PublicKeyAccount {
  val empty = apply(Array.emptyByteArray)

  val KeyStringLength: Int = base58Length(KeyLength)

  private case class PublicKeyAccountImpl(publicKey: Array[Byte]) extends PublicKeyAccount

  def apply(publicKey: Array[Byte]): PublicKeyAccount = PublicKeyAccountImpl(publicKey)

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address = Address.fromPublicKey(publicKeyAccount.publicKey)

  implicit class PublicKeyAccountExt(pk: PublicKeyAccount) {
    def toAddress: Address = PublicKeyAccount.toAddress(pk)
  }

  def fromBase58String(s: String): Either[InvalidAddress, PublicKeyAccount] =
    (for {
      _     <- Either.cond(s.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(s).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKeyAccount(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))
}
