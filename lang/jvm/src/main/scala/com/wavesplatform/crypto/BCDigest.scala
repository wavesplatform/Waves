package com.wavesplatform.crypto

import org.bouncycastle.crypto.Digest

private[crypto] abstract class BCDigest(initial: () => Digest, digestSize: Int) {
  protected val digest: ThreadLocal[Digest] = new ThreadLocal[Digest] {
    override def initialValue(): Digest = initial()
  }

  def hash(message: Array[Byte]): Array[Byte] = {
    val d = digest.get()
    d.update(message, 0, message.length)
    val result = new Array[Byte](digestSize)
    d.doFinal(result, 0)
    result
  }
}
