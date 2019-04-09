package com.wavesplatform.crypto

import com.wavesplatform.account.KeyPair
import com.wavesplatform.crypto
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class SigningFunctionsSpecification extends PropSpec with PropertyChecks with Matchers {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte], message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc      = KeyPair(seed1)
        val sig      = crypto.sign(acc, message1)
        val rightKey = acc
        crypto.verify(sig, message1, rightKey.publicKey) should be(true)

        val wrongKey = KeyPair(seed2)
        crypto.verify(sig, message1, wrongKey) shouldNot be(true)

        crypto.verify(sig, message2, rightKey) shouldNot be(true)
      }
    }
  }
}
