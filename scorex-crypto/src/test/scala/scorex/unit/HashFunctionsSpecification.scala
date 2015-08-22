package scorex.unit

import org.scalatest.{FunSuite, Matchers}


class HashFunctionsSpecification  extends FunSuite with Matchers {
  import scorex.crypto.HashFunctionsImpl._

  val testBytes = "hello world".getBytes

  private def bytes2hex(bytes: Array[Byte]): String = bytes2hex(bytes, None)

  private def bytes2hex(bytes: Array[Byte], sep: Option[String]): String =
    bytes.map("%02x".format(_)).mkString(sep.getOrElse(""))

  //todo: props test
  test("doublesha test") {
    assert(doubleHash(testBytes).sameElements(hash(hash(testBytes))))
  }

  //todo: works for sha256 only
  test("hash comparing with externally computed value") {
    //checking sha256 result with http://www.xorbin.com/tools/sha256-hash-calculator
    assert(bytes2hex(hash(testBytes)) == "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9")
  }
}
