package com.wavesplatform.it

import com.google.common.primitives.Ints
import com.wavesplatform.account.KeyPair

import java.security.SecureRandom
import java.util.concurrent.atomic.AtomicInteger

object RandomKeyPair {
  private[this] val counter = new AtomicInteger(SecureRandom.getInstanceStrong.nextInt())

  def apply(): KeyPair = KeyPair(Ints.toByteArray(counter.incrementAndGet()))
}
