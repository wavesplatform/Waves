package com.wavesplatform.account

import java.util

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.Curve25519
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils
import play.api.libs.json.{Format, Json, Writes}

import scala.util.{Failure, Success}

final class KeyPair(val seed: Array[Byte]) {
  lazy val privateKey: PrivateKey = PrivateKey(Curve25519.privateKeyFromSeed(seed))
  lazy val publicKey: PublicKey   = PublicKey(Curve25519.publicKeyFromPrivateKey(privateKey.arr))

  override def equals(obj: Any): Boolean = obj match {
    case kp: KeyPair => util.Arrays.equals(kp.seed, seed)
    case _           => false
  }

  private lazy val hc          = util.Arrays.hashCode(seed)
  override def hashCode(): Int = hc
}

object KeyPair {
  def apply(seed: ByteStr): KeyPair     = new KeyPair(seed.arr)
  def apply(seed: Array[Byte]): KeyPair = new KeyPair(seed)

  def fromSeed(base58: String): Either[GenericError, KeyPair] = Base58.tryDecodeWithLimit(base58) match {
    case Success(x) => Right(KeyPair(ByteStr(x)))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$base58': ${e.getMessage}"))
  }

  implicit class KeyPairImplicitOps(private val kp: KeyPair) extends AnyVal {
    def toAddress: WavesAddress                = kp.publicKey.toAddress
    def toAddress(chainId: Byte): WavesAddress = kp.publicKey.toAddress(chainId)
  }

  implicit val jsonFormat: Format[KeyPair] = Format(
    utils.byteStrFormat.map(KeyPair(_)),
    Writes(v => Json.obj("seed" -> Base58.encode(v.seed), "publicKey" -> v.publicKey, "privateKey" -> v.privateKey))
  )
}
