package com.wavesplatform.account

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.ValidationError.InvalidAddress
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{Format, Writes}
import supertagged._

object AccountPublicKey extends TaggedType[ByteStr] {
  val KeyStringLength: Int = base58Length(KeyLength)

  val empty = apply(ByteStr.empty)

  def apply(publicKey: ByteStr): AccountPublicKey =
    ByteStr(publicKey) @@ AccountPublicKey

  def apply(publicKey: Array[Byte]): AccountPublicKey =
    apply(ByteStr(publicKey))

  def unapply(arg: AccountPublicKey): Option[Array[Byte]] =
    Some(arg)

  def fromBase58String(base58: String): Either[InvalidAddress, AccountPublicKey] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield AccountPublicKey(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))

  implicit def toAddress(AccountPublicKey: AccountPublicKey): Address =
    AccountPublicKey.toAddress

  implicit class AccountPublicKeyImplicitOps(private val pk: AccountPublicKey) extends AnyVal {
    def toAddress: Address = Address.fromPublicKey(pk)
  }

  implicit lazy val jsonFormat: Format[AccountPublicKey] = Format[AccountPublicKey](
    com.wavesplatform.utils.byteStrWrites.map(this.apply),
    Writes(pk => com.wavesplatform.utils.byteStrWrites.writes(pk))
  )
}
