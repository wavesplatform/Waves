package com.wavesplatform.crypto.bls

import com.wavesplatform.common.state.ByteStr

case class BlsSignature private (byteStr: ByteStr) extends AnyVal {
  def arr: Array[Byte] = byteStr.arr

  def base64: String            = byteStr.base64
  override def toString: String = byteStr.base64Raw
}

object BlsSignature {
  val SizeInBytes = 98
  val empty = BlsSignature(Array.empty[Byte])

  // TODO: check size?
  def apply(arr: Array[Byte]): BlsSignature = new BlsSignature(ByteStr(arr))
  def apply(byteStr: ByteStr): BlsSignature = new BlsSignature(byteStr)
}
