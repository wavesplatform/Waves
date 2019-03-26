package com.wavesplatform.account

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.ValidationError.GenericError
import supertagged._

object AccountPrivateKey extends TaggedType[ByteStr] {
  def apply(privateKey: ByteStr): AccountPrivateKey = {
    privateKey @@ AccountPrivateKey
  }

  def apply(privateKey: Array[Byte]): AccountPublicKey =
    apply(ByteStr(privateKey))

  @deprecated("Use AccountKeyPair.fromSeed", "0.17.0")
  def fromSeed(seed: ByteStr): AccountPrivateKey =
    AccountKeyPair(seed).privateKey

  @deprecated("Use AccountKeyPair.fromSeed", "0.17.0")
  def fromSeed(base58: String): Either[GenericError, AccountPrivateKey] =
    AccountKeyPair.fromSeed(base58).map(_.privateKey)
}
