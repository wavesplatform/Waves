package com.wavesplatform.account

import com.google.common.collect.Interners
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.transaction.ValidationError.GenericError
import supertagged._

import scala.util.{Failure, Success}

final case class KeyPair(seed: ByteStr) {
  private[this] lazy val pair = crypto.createKeyPair(seed)

  lazy val privateKey: PrivateKey = ByteStr(pair._1) @@ PrivateKey
  lazy val publicKey: PublicKey   = ByteStr(pair._2) @@ PublicKey
}

object KeyPair {
  private[this] val interner = Interners.newWeakInterner[KeyPair]()

  def apply(seed: ByteStr): KeyPair =
    interner.intern(new KeyPair(seed))

  def fromSeed(base58: String): Either[GenericError, KeyPair] = Base58.tryDecodeWithLimit(base58) match {
    case Success(x) => Right(KeyPair(x))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$base58': ${e.getMessage}"))
  }

  implicit class KeyPairImplicitOps(private val kp: KeyPair) extends AnyVal {
    def toAddress: Address = PublicKey.toAddress(kp)
  }

  implicit def toPublicKey(kp: KeyPair): PublicKey   = kp.publicKey
  implicit def toPrivateKey(kp: KeyPair): PrivateKey = kp.privateKey
  implicit def toAddress(keyPair: KeyPair): Address         = keyPair.toAddress
}
