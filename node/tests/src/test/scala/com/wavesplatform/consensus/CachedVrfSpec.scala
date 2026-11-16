package com.wavesplatform.consensus

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.crypto
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.utils.*

class CachedVrfSpec extends FreeSpec {
  private val account = KeyPair("cached-vrf-spec".utf8Bytes)
  private val message = "message".utf8Bytes
  private val proof   = crypto.signVRF(account.privateKey, message)

  "returns what an uncached check returns, again on a repeated check" in {
    val expected = crypto.verifyVRF(proof, message, account.publicKey)
    expected should be(Symbol("right"))
    CachedVrf.verify(proof, message, account.publicKey, checkWeakPk = true) shouldBe expected
    CachedVrf.verify(proof, message, account.publicKey, checkWeakPk = true) shouldBe expected
  }

  "a proof that passed under its key fails under another key or message" in {
    CachedVrf.verify(proof, message, account.publicKey, checkWeakPk = false) should be(Symbol("right"))
    CachedVrf.verify(proof, message, KeyPair("other".utf8Bytes).publicKey, checkWeakPk = false) shouldBe crypto.verifyVRF(
      proof,
      message,
      KeyPair("other".utf8Bytes).publicKey
    )
    CachedVrf.verify(proof, "other".utf8Bytes, account.publicKey, checkWeakPk = false) should be(Symbol("left"))
  }

  "changing a returned output or a checked message does not change what is cached" in {
    val expected = crypto.verifyVRF(proof, message, account.publicKey).explicitGet()
    val output   = CachedVrf.verify(proof, message, account.publicKey, checkWeakPk = false).explicitGet()
    output.arr(0) = (output.arr(0) + 1).toByte

    val changingMessage = message.clone()
    CachedVrf.verify(proof, changingMessage, account.publicKey, checkWeakPk = false)
    changingMessage(0) = (changingMessage(0) + 1).toByte

    CachedVrf.verify(proof, message, account.publicKey, checkWeakPk = false).explicitGet() shouldBe expected
  }
}
