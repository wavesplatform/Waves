package com.wavesplatform.account

import com.google.common.collect.Interners
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto._
import com.wavesplatform.transaction.ValidationError.InvalidAddress
import com.wavesplatform.utils.base58Length
import play.api.libs.json.{Format, Writes}
import supertagged._

object PublicKey extends TaggedType[ByteStr] {
  private[this] final class InternedPublicKey(val publicKey: PublicKey) {
    lazy val address = Address.fromPublicKey(publicKey)

    override def hashCode(): Int           = publicKey.hashCode()
    override def equals(obj: Any): Boolean = obj match {
      case that: InternedPublicKey => this.publicKey == that.publicKey
      case _ => publicKey.equals(obj)
    }
  }

  private[this] val interner = Interners.newWeakInterner[InternedPublicKey]()

  val KeyStringLength: Int = base58Length(KeyLength)

  val empty = apply(ByteStr.empty)

  def apply(publicKey: ByteStr): PublicKey =
    interner.intern(new InternedPublicKey(ByteStr(publicKey) @@ PublicKey)).publicKey

  def apply(publicKey: Array[Byte]): PublicKey =
    apply(ByteStr(publicKey))

  def unapply(arg: PublicKey): Option[Array[Byte]] =
    Some(arg)

  def fromBase58String(base58: String): Either[InvalidAddress, PublicKey] =
    (for {
      _     <- Either.cond(base58.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.tryDecodeWithLimit(base58).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKey(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))

  implicit def toAddress(pk: PublicKey): Address =
    pk.toAddress

  implicit class PublicKeyImplicitOps(private val pk: PublicKey) extends AnyVal {
    def toAddress: Address = interner.intern(new InternedPublicKey(pk)).address
  }

  implicit lazy val jsonFormat: Format[PublicKey] = Format[PublicKey](
    com.wavesplatform.utils.byteStrWrites.map(this.apply),
    Writes(pk => com.wavesplatform.utils.byteStrWrites.writes(pk))
  )
}
