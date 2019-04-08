package com.wavesplatform.account

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.EitherExt2
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AccountOrAliasSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Account serialization round trip") {
    forAll(accountGen) { account =>
      val bytes   = account.bytes.arr
      val address = Address.fromBytes(bytes).explicitGet()
      address.stringRepr shouldBe account.stringRepr
    }
  }

  property("Alias serialization round trip") {
    forAll(aliasGen) { alias: Alias =>
      val bytes          = alias.bytes.arr
      val representation = Alias.fromBytes(bytes).explicitGet()
      representation.stringRepr shouldBe representation.stringRepr
    }
  }

  property("AccountOrAlias serialization round trip") {
    forAll(accountOrAliasGen) { aoa: AddressOrAlias =>
      val bytes          = aoa.bytes.arr
      val addressOrAlias = AddressOrAlias.fromBytes(bytes, 0).explicitGet()
      addressOrAlias._1.stringRepr shouldBe aoa.stringRepr
    }
  }
}
