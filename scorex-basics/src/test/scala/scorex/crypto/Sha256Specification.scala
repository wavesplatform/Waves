package scorex.crypto

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.Sha256._

class Sha256Specification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("doublehash(x) is hash(hash(x))") {
    forAll { data: Array[Byte] =>
      doubleHash(data) should equal (hash(hash(data)))
    }
  }

  property("no collisions") {
    forAll { (x: Array[Byte], y: Array[Byte]) =>
      whenever(!x.sameElements(y)) {
        hash(x) should not equal hash(y)
      }
    }
  }

  private def bytes2hex(bytes: Array[Byte]): String = bytes2hex(bytes, None)

  private def bytes2hex(bytes: Array[Byte], sep: Option[String]): String =
    bytes.map("%02x".format(_)).mkString(sep.getOrElse(""))

  property("hash comparing with externally computed value") {
    //checking sha256 result with http://www.xorbin.com/tools/sha256-hash-calculator
    assert(bytes2hex(hash("hello world".getBytes)) == "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9")

    //test samples from a Qeditas unit test
    assert(bytes2hex(hash("")) == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assert(bytes2hex(hash("abc")) == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assert(bytes2hex(hash("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")) ==
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
  }
}
