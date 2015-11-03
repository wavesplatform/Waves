package scorex.basics.props

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
}
