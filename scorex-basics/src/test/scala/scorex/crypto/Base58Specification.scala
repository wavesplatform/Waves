package scorex.crypto

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class Base58Specification extends PropSpec
with PropertyChecks
with GeneratorDrivenPropertyChecks
with Matchers {

  property("Base58 encoding then decoding preserves data") {
    forAll { data: Array[Byte] =>
      whenever(data.length > 0 && data.head != 0) {
        val encoded = Base58.encode(data)
        val restored = Base58.decode(encoded).get
        restored shouldBe data
      }
    }
  }

  property("base58 sample") {
    val b58 = "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"
    Base58.encode(Base58.decode(b58).get) shouldBe b58
  }
}
