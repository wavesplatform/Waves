package com.wavesplatform.crypto

import java.security.MessageDigest

object Sha256 {
  private val digest = new ThreadLocal[MessageDigest]() {
    override def initialValue(): MessageDigest = MessageDigest.getInstance("SHA-256")
  }

  def hash(message: Array[Byte]): Array[Byte] = {
    val result = digest.get().digest(message)
    digest.get().reset()
    result
  }
}
