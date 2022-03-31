package com.wavesplatform.test

import java.util.concurrent.ThreadLocalRandom

import com.google.common.primitives.Longs
import com.wavesplatform.account.KeyPair

object RandomKeyPair {
  def apply(): KeyPair =
    KeyPair(Longs.toByteArray(ThreadLocalRandom.current().nextLong()))
}
