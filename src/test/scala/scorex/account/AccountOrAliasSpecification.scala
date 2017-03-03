package scorex.account

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.transaction.TransactionGen

class AccountOrAliasSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("Account serialization roundtrip") {
    forAll(accountGen) { acc: PrivateKeyAccount =>
      val ser = acc.bytes
      val deser = Account.fromBytes(ser).right.get
      deser.stringRepr shouldBe acc.stringRepr
    }
  }

  property("Alias serialization roundtrip") {
    forAll(aliasGen) { alias: Alias =>
      val ser = alias.bytes
      val deser = Alias.fromBytes(ser).right.get
      deser.stringRepr shouldBe alias.stringRepr
    }
  }

  property("AccountOrAlias serialization roundtrip") {
    forAll(accountOrAliasGen) { aoa: AccountOrAlias =>
      val ser = aoa.bytes
      val deser = AccountOrAlias.fromBytes(ser, 0).right.get
      deser._1.stringRepr shouldBe aoa.stringRepr
    }
  }
}
