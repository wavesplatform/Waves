package scorex.basics.props

import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.Base58

class Base58Specification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("Base58 encoding then decoding preserves data") {

    forAll { data: Array[Byte] =>
      whenever (data.length>0 && data.head!=0) {
        val encoded = Base58.encode(data)
        val restored = Base58.decode(encoded).get
        restored shouldBe data
      }
    }
  }
}
