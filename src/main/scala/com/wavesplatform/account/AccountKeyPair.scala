package com.wavesplatform.account

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.transaction.ValidationError.GenericError
import supertagged._

import scala.util.{Failure, Success}

final case class AccountKeyPair(seed: ByteStr) {
  private[this] lazy val pair = crypto.createKeyPair(seed)

  lazy val privateKey: AccountPrivateKey = ByteStr(pair._1) @@ AccountPrivateKey
  lazy val publicKey: AccountPublicKey   = ByteStr(pair._2) @@ AccountPublicKey
}

object AccountKeyPair {
  def fromSeed(base58: String): Either[GenericError, AccountKeyPair] = Base58.tryDecodeWithLimit(base58) match {
    case Success(x) => Right(AccountKeyPair(x))
    case Failure(e) => Left(GenericError(s"Unable to get a private key from the seed '$base58': ${e.getMessage}"))
  }

  implicit class AccountKeyPairImplicitOps(private val kp: AccountKeyPair) extends AnyVal {
    def toAddress: Address = AccountPublicKey.toAddress(kp)
  }

  implicit def toAccountPublicKey(kp: AccountKeyPair): AccountPublicKey   = kp
  implicit def toAccountPrivateKey(kp: AccountKeyPair): AccountPrivateKey = kp.privateKey
  implicit def toAddress(keyPair: AccountKeyPair): Address                = keyPair.toAddress
}
