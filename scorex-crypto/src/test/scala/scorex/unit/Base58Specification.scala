package scorex.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.crypto.Base58

import scala.util.Random


class Base58Specification extends FunSuite with Matchers {
  test("base58 roundtrip") {
    val b58 = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
    Base58.encode(Base58.decode(b58).get) shouldBe b58
  }

  //todo: made props test like that
  test("base58 encode and decode") {
    val data = Random.nextString(50).getBytes
    val encoded = Base58.encode(data)
    val restored = Base58.decode(encoded).get
    restored shouldBe data
  }
}
