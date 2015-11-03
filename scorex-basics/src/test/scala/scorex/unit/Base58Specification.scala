package scorex.lagonaki.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.Base58

class Base58Specification extends FunSuite with Matchers {
  test("base58 roundtrip") {
    val b58 = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
    Base58.encode(Base58.decode(b58).get) shouldBe b58
  }
}
