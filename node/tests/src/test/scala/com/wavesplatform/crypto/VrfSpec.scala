package com.wavesplatform.crypto

import com.wavesplatform.account.KeyPair
import com.wavesplatform.crypto
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.*

class VrfSpec extends FreeSpec {
  private val account = KeyPair("vrf-spec".utf8Bytes)
  private val message = "message".utf8Bytes
  private val proof   = crypto.signVRF(account.privateKey, message)

  "a repeated check of a valid proof returns the same output" in {
    val first = crypto.verifyVRF(proof, message, account.publicKey)
    first should be(Symbol("right"))
    crypto.verifyVRF(proof, message, account.publicKey) shouldBe first
  }

  "a proof checked against another message keeps failing" in {
    val otherMessage = "other".utf8Bytes
    crypto.verifyVRF(proof, otherMessage, account.publicKey) should be(Symbol("left"))
    crypto.verifyVRF(proof, otherMessage, account.publicKey) should be(Symbol("left"))
  }
}
