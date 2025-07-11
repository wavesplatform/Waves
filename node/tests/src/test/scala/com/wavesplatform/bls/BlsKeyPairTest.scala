package com.wavesplatform.bls

import com.wavesplatform.account.KeyPair
import com.wavesplatform.test.FreeSpec

import scala.util.Random

class BlsKeyPairTest extends FreeSpec {
  "sig/verify" in {
    val wavesKP = KeyPair(Array.fill(32)(Random.nextInt().toByte))
    val blsKP   = BlsKeyPair(wavesKP.privateKey)

    val message   = "assertion".getBytes();
    val signature = blsKP.sign(message)
    blsKP.verify(message, signature) shouldBe true
  }
}
