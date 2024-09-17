package com.wavesplatform.account

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.test.PropSpec

class AccountOrAliasSpecification extends PropSpec {

  property("Account serialization round trip") {
    forAll(accountGen) { account =>
      val bytes   = account.toAddress.bytes
      val address = Address.fromBytes(bytes).explicitGet()
      address shouldBe account.toAddress
    }
  }

  property("Alias serialization round trip") {
    forAll(aliasGen) { (alias: Alias) =>
      val bytes          = alias.bytes
      val representation = Alias.fromBytes(bytes, None).explicitGet()
      representation.toString shouldBe representation.toString
    }
  }

  property("AccountOrAlias serialization round trip") {
    forAll(accountOrAliasGen) { (aoa: AddressOrAlias) =>
      val bytes          = aoa.bytes
      val addressOrAlias = AddressOrAlias.fromBytes(bytes).explicitGet()
      addressOrAlias.toString shouldBe aoa.toString
    }
  }
}
