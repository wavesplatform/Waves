package scorex.account

import com.wavesplatform.TransactionGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class AccountOrAliasSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Account serialization round trip") {
    forAll(accountGen) { account: PrivateKeyAccount =>
      val bytes = account.bytes.arr
      val address = Address.fromBytes(bytes).right.get
      address.stringRepr shouldBe account.stringRepr
    }
  }

  property("Alias serialization round trip") {
    forAll(aliasGen) { alias: Alias =>
      val bytes = alias.bytes.arr
      val representation = Alias.fromBytes(bytes).right.get
      representation.stringRepr shouldBe representation.stringRepr
    }
  }

  property("AccountOrAlias serialization round trip") {
    forAll(accountOrAliasGen) { aoa: AddressOrAlias =>
      val bytes = aoa.bytes.arr
      val addressOrAlias = AddressOrAlias.fromBytes(bytes, 0).right.get
      addressOrAlias._1.stringRepr shouldBe aoa.stringRepr
    }
  }
}
