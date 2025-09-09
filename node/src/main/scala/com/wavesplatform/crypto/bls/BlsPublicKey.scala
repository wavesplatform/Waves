package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr

case class BlsPublicKey private (byteStr: ByteStr) extends AnyVal {
  def arr: Array[Byte] = byteStr.arr

  def verify(message: Array[Byte], signature: BlsSignature.NonEmpty): Boolean =
    BlsUtils.verifyBasic(signature.arr, message, arr)

  def base64: String            = byteStr.base64
  override def toString: String = byteStr.base64Raw
}

object BlsPublicKey {
  val SizeInBytes = 48

  // TODO: check size
  def apply(arr: Array[Byte]): BlsPublicKey = new BlsPublicKey(ByteStr(arr))
  def apply(byteStr: ByteStr): BlsPublicKey = new BlsPublicKey(byteStr)
}
