package com.wavesplatform.crypto

import java.security.MessageDigest

object Sha256 {
  private val digest = ThreadLocal.withInitial(() => MessageDigest.getInstance("SHA-256", Provider.name))

  def hash(message: Array[Byte]): Array[Byte] = digest.get().digest(message)
}
