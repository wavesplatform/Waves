package scorex.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.crypto.SigningFunctionsImpl

import scala.util.Random

class SigningFunctionsSpecification extends FunSuite with Matchers {

  //todo: props test?
  test("sign then verify") {
    val acc = new PrivateKeyAccount(Random.nextString(20).getBytes)
    val data = Random.nextString(30).getBytes

    val sig = SigningFunctionsImpl.sign(acc, data)
    val rightKey = acc.publicKey
    assert(SigningFunctionsImpl.verify(sig, data, rightKey))

    val wrongKey = new PrivateKeyAccount(Random.nextString(20).getBytes).publicKey
    assert(!SigningFunctionsImpl.verify(sig, data, wrongKey))

    val wrongData = data ++ Seq(0:Byte)
    assert(!SigningFunctionsImpl.verify(sig, wrongData, rightKey))
  }
}