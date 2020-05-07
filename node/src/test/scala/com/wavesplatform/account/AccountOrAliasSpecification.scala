package com.wavesplatform.account

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.EitherExt2
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AccountOrAliasSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Account serialization round trip") {
    forAll(accountGen) { account =>
      val bytes   = account.toAddress.bytes
      val address = Address.fromBytes(bytes).explicitGet()
      address shouldBe account.toAddress
    }
  }

  property("Alias serialization round trip") {
    forAll(aliasGen) { alias: Alias =>
      val bytes          = alias.bytes
      val representation = Alias.fromBytes(bytes).explicitGet()
      representation.stringRepr shouldBe representation.stringRepr
    }
  }

  property("AccountOrAlias serialization round trip") {
    forAll(accountOrAliasGen) { aoa: AddressOrAlias =>
      val bytes          = aoa.bytes
      val addressOrAlias = AddressOrAlias.fromBytes(bytes, 0).explicitGet()
      addressOrAlias._1.stringRepr shouldBe aoa.stringRepr
    }
  }
}
