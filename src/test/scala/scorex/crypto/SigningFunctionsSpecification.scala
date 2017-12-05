package scorex.crypto

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount


class SigningFunctionsSpecification extends PropSpec
with PropertyChecks
with Matchers {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte],
              message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc = PrivateKeyAccount(seed1)
        val sig = EllipticCurveImpl.sign(acc, message1)
        val rightKey = acc.publicKey
        EllipticCurveImpl.verify(sig, message1, rightKey) should be (true)

        val wrongKey = PrivateKeyAccount(seed2).publicKey
        EllipticCurveImpl.verify(sig, message1, wrongKey) shouldNot be (true)

        EllipticCurveImpl.verify(sig, message2, rightKey) shouldNot be (true)
      }
    }
  }
}