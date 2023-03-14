package com.wavesplatform.account

import java.util

import scala.util.{Failure, Success}

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto.Curve25519
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils
import play.api.libs.json.{Format, Json, Writes}

sealed trait KeyPair {
  def privateKey: PrivateKey
  def publicKey: PublicKey
}

object KeyPair {
  implicit class KeyPairImplicitOps(private val kp: KeyPair) extends AnyVal {
    def toAddress: Address                = kp.publicKey.toAddress
    def toAddress(chainId: Byte): Address = kp.publicKey.toAddress(chainId)
  }

  def apply(privateKey: PrivateKey): PKKeyPair = PKKeyPair(privateKey)
  def apply(seed: ByteStr): SeedKeyPair        = SeedKeyPair(seed.arr)
  def apply(seed: Array[Byte]): SeedKeyPair    = SeedKeyPair(seed)
  def fromSeed(base58: String): Either[GenericError, SeedKeyPair] = Base58.tryDecodeWithLimit(base58) match {
    case Success(x) => Right(SeedKeyPair(ByteStr(x)))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$base58': ${e.getMessage}"))
  }
}

final class SeedKeyPair(val seed: Array[Byte]) extends KeyPair {
  lazy val privateKey: PrivateKey = PrivateKey(Curve25519.privateKeyFromSeed(seed))
  lazy val publicKey: PublicKey   = PublicKey(Curve25519.publicKeyFromPrivateKey(privateKey.arr))

  override def equals(obj: Any): Boolean = obj match {
    case kp: SeedKeyPair => util.Arrays.equals(kp.seed, seed)
    case _               => false
  }

  private lazy val hc          = util.Arrays.hashCode(seed)
  override def hashCode(): Int = hc
}

object SeedKeyPair {
  def apply(seed: ByteStr): SeedKeyPair     = new SeedKeyPair(seed.arr)
  def apply(seed: Array[Byte]): SeedKeyPair = new SeedKeyPair(seed)

  implicit val jsonFormat: Format[SeedKeyPair] = Format(
    utils.byteStrFormat.map(SeedKeyPair(_)),
    Writes(v => Json.obj("seed" -> Base58.encode(v.seed), "publicKey" -> v.publicKey, "privateKey" -> v.privateKey))
  )
}

final class PKKeyPair(val privateKey: PrivateKey) extends KeyPair {
  lazy val publicKey: PublicKey = PublicKey(Curve25519.publicKeyFromPrivateKey(privateKey.arr))

  override def equals(obj: Any): Boolean = obj match {
    case kp: PKKeyPair => util.Arrays.equals(kp.privateKey.arr, privateKey.arr)
    case _             => false
  }

  private lazy val hc          = util.Arrays.hashCode(privateKey.arr)
  override def hashCode(): Int = hc
}

object PKKeyPair {
  def apply(privateKey: PrivateKey): PKKeyPair = new PKKeyPair(privateKey)
}
