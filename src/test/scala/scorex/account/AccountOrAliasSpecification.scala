package scorex.account

import com.wavesplatform.TransactionGen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class AccountOrAliasSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("Account serialization roundtrip") {
    forAll(accountGen) { acc: PrivateKeyAccount =>
      val ser = acc.bytes.arr
      val deser = Address.fromBytes(ser).right.get
      deser.stringRepr shouldBe acc.stringRepr
    }
  }

  property("Alias serialization roundtrip") {
    forAll(aliasGen) { alias: Alias =>
      val ser = alias.bytes.arr
      val deser = Alias.fromBytes(ser).right.get
      deser.stringRepr shouldBe alias.stringRepr
    }
  }

  property("AccountOrAlias serialization roundtrip") {
    forAll(accountOrAliasGen) { aoa: AddressOrAlias =>
      val ser = aoa.bytes.arr
      val deser = AddressOrAlias.fromBytes(ser, 0).right.get
      deser._1.stringRepr shouldBe aoa.stringRepr
    }
  }
}
