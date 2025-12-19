package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxValidationError.GenericError

opaque type BlsPublicKey = ByteStr

object BlsPublicKey {
  val SizeInBytes = 48

  extension (self: BlsPublicKey) {
    def byteStr: ByteStr = self
    def arr: Array[Byte] = byteStr.arr

    def verify(message: Array[Byte], signature: BlsSignature): Boolean = BlsUtils.verifyBasic(signature.arr, message, arr)

    def base58: String = byteStr.toString
  }

  private[bls] def unsafe(byteStr: ByteStr): BlsPublicKey = byteStr

  def apply(arr: Array[Byte]): Either[GenericError, BlsPublicKey] = apply(ByteStr(arr))
  def apply(byteStr: ByteStr): Either[GenericError, BlsPublicKey] = Either.cond(
    byteStr.arr.length == SizeInBytes,
    byteStr,
    GenericError(s"Unexpected BLS public key length: ${byteStr.arr.length}, expected: $SizeInBytes")
  )
}
