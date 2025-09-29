package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr

case class BlsPublicKey private (byteStr: ByteStr) extends AnyVal {
  def arr: Array[Byte] = byteStr.arr

  def verify(message: Array[Byte], signature: BlsSignature.NonEmpty): Boolean =
    BlsUtils.verifyBasic(signature.arr, message, arr)

  def base58: String            = byteStr.toString
  override def toString: String = byteStr.toString
}

object BlsPublicKey {
  val SizeInBytes = 48

  // TODO: check size
  def apply(arr: Array[Byte]): BlsPublicKey = new BlsPublicKey(ByteStr(arr))
  def apply(byteStr: ByteStr): BlsPublicKey = new BlsPublicKey(byteStr)
}
