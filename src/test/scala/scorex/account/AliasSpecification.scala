package scorex.account

import com.wavesplatform.TransactionGen
import org.scalacheck.Shrink
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AliasSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
  with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  property("Correct alias should be valid") {
    forAll(validAliasStringGen) { s =>
      Alias.buildWithCurrentNetworkByte(s) shouldBe 'right
    }
  }

  property("Incorrect alias should be invalid") {
    forAll(invalidAliasStringGen) { s =>
      Alias.buildWithCurrentNetworkByte(s) shouldBe 'left
    }
  }
}
