package scorex.test

import org.scalatest.FunSuite
import scorex.account.PrivateKeyAccount
import scorex.crypto.Crypto

import scala.util.Random

class CryptoSpecification extends FunSuite {

  test("sign then verify") {
    val acc = new PrivateKeyAccount(Random.nextString(20).getBytes)

    val data = Random.nextString(30).getBytes

    val sig = Crypto.sign(acc, data)
    assert(Crypto.verify(sig, data, acc.publicKey))
  }
}