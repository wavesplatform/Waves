package scorex.account

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AliasSpecification extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

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
