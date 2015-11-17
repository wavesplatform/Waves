package scorex.props

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.crypto.Curve25519


class SigningFunctionsSpecification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("signed message should be verifiable with appropriate public key") {
    forAll { (seed1: Array[Byte], seed2: Array[Byte],
              message1: Array[Byte], message2: Array[Byte]) =>
      whenever(!seed1.sameElements(seed2) && !message1.sameElements(message2)) {
        val acc = new PrivateKeyAccount(seed1)
        val sig = Curve25519.sign(acc, message1)
        val rightKey = acc.publicKey
        Curve25519.verify(sig, message1, rightKey) should be (true)

        val wrongKey = new PrivateKeyAccount(seed2).publicKey
        Curve25519.verify(sig, message1, wrongKey) shouldNot be (true)

        Curve25519.verify(sig, message2, rightKey) shouldNot be (true)
      }
    }
  }
}