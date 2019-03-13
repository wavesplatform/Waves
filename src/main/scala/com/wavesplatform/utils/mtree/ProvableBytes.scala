package com.wavesplatform.utils.mtree

import com.wavesplatform.serialization.BytesSerializable
import scorex.crypto.hash.{CryptographicHash, Digest32}

trait ProvableBytes[A] {
  def provableBytes(a: A): Array[Byte]
}

object ProvableBytes {
  def apply[A](implicit pr: ProvableBytes[A]): ProvableBytes[A] = pr

  implicit def byteSerProvable[A <: BytesSerializable](
      implicit
      ch: CryptographicHash[Digest32]
  ): ProvableBytes[A] = (a: A) => a.bytes()
}
