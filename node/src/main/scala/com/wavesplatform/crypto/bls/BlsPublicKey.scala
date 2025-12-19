package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxValidationError.GenericError

case class BlsPublicKey private (byteStr: ByteStr) extends AnyVal {
  def arr: Array[Byte] = byteStr.arr

  def verify(message: Array[Byte], signature: BlsSignature): Boolean =
    BlsUtils.verifyBasic(signature.arr, message, arr)

  def base58: String            = byteStr.toString
  override def toString: String = byteStr.toString
}

object BlsPublicKey {
  val SizeInBytes = 48

  private[bls] def unsafe(byteStr: ByteStr): BlsPublicKey = new BlsPublicKey(byteStr)

  def apply(arr: Array[Byte]): Either[GenericError, BlsPublicKey] = apply(ByteStr(arr))
  def apply(byteStr: ByteStr): Either[GenericError, BlsPublicKey] =
    Either.cond(
      byteStr.arr.length == SizeInBytes,
      new BlsPublicKey(byteStr),
      GenericError(s"Unexpected BLS public key length: ${byteStr.arr.length}, expected: $SizeInBytes")
    )
}
