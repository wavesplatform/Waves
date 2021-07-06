package com.wavesplatform.crypto

import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.test.PropSpec

class SigningFunctionsSpecification extends PropSpec {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc      = KeyPair(ByteStr(seed1))
        val sig      = crypto.sign(acc.privateKey, message1)
        val rightKey = acc
        crypto.verify(sig, message1, rightKey.publicKey) should be(true)

        val wrongKey = KeyPair(ByteStr(seed2))
        crypto.verify(sig, message1, wrongKey.publicKey) shouldNot be(true)

        crypto.verify(sig, message2, rightKey.publicKey) shouldNot be(true)
      }
    }
  }
}
