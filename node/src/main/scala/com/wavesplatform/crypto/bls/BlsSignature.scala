package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxValidationError.GenericError

opaque type BlsSignature = ByteStr

object BlsSignature {
  val SizeInBytes = 96

  extension (self: BlsSignature) {
    def byteStr: ByteStr = self
    def arr: Array[Byte] = byteStr.arr
    def base58: String   = byteStr.toString

    def verifyAgg(message: Array[Byte], blsPks: Iterable[BlsPublicKey]): Either[String, Boolean] =
      BlsUtils.verifyAgg(byteStr.arr, message, blsPks.map(_.arr))

    def append(other: BlsSignature): BlsSignature = ByteStr(BlsUtils.aggSign(self.arr, other.arr))
  }

  private[bls] def unsafe(byteStr: ByteStr): BlsSignature = byteStr

  def apply(arr: Array[Byte]): Either[GenericError, BlsSignature] = apply(ByteStr(arr))
  def apply(byteStr: ByteStr): Either[GenericError, BlsSignature] = Either.cond(
    byteStr.arr.length == SizeInBytes,
    byteStr,
    GenericError(s"Unexpected BLS signature length: ${byteStr.arr.length}, expected: $SizeInBytes")
  )
}
