package com.wavesplatform.consensus

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.util.Try

/** VRF proof checks that remember successful results: block validation checks each proof twice (`validateGenerationSignature`,
  * `validateBlockDelay`), and the importer checks proofs ahead of the appender. A failed check is not remembered. Same results and errors as
  * [[crypto.verifyVRF]].
  */
object CachedVrf {
  private val VerificationFailed = GenericError("Could not verify VRF proof")

  // Holds the importer's read-ahead (--max-queue-size, 100 blocks by default) many times over.
  private val outputs: LoadingCache[(ByteStr, ByteStr, PublicKey), ByteStr] =
    CacheBuilder
      .newBuilder()
      .maximumSize(1024)
      .build(CacheLoader.from { (key: (ByteStr, ByteStr, PublicKey)) =>
        val (signature, message, publicKey) = key
        crypto.verifyVRF(signature, message.arr, publicKey).fold(e => throw new IllegalArgumentException(e.toString), identity)
      })

  /** The message is copied into the key and the output out of the cache, so a caller that changes its array cannot change a cached entry. */
  def verify(signature: ByteStr, message: Array[Byte], publicKey: PublicKey, checkWeakPk: Boolean): Either[ValidationError, ByteStr] =
    if (checkWeakPk && crypto.isWeakPublicKey(publicKey.arr)) crypto.verifyVRF(signature, message, publicKey, checkWeakPk)
    else
      Try(outputs.getUnchecked((signature, ByteStr(message.clone()), publicKey))).toEither
        .map(output => ByteStr(output.arr.clone()))
        .left
        .map(_ => VerificationFailed)
}
