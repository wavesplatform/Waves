package com.wavesplatform.account

import com.wavesplatform.common.utils.EitherExt2
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AccountOrAliasTests extends PropSpec with PropertyChecks with Matchers with EitherValues {

  property("Account should get parsed correctly") {
    AddressOrAlias.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet() shouldBe an[Address]
    AddressOrAlias.fromString("address:3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet() shouldBe an[Address]

    Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet() shouldBe an[Address]
    Address.fromString("address:3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet() shouldBe an[Address]
  }

  property("Alias should get parsed correctly") {
    val alias = AddressOrAlias.fromString("alias:T:sasha").explicitGet().asInstanceOf[Alias]
    alias.name shouldBe "sasha"
    alias.chainId shouldBe 'T'

    val alias2 = Alias.fromString("alias:T:sasha").explicitGet()
    alias2.name shouldBe "sasha"
    alias2.chainId shouldBe 'T'

  }

  property("Alias can be from other network") {
    AddressOrAlias.fromString("alias:Q:sasha") shouldBe Alias.createWithChainId("sasha", 'Q'.toByte)
  }

  property("Malformed aliases cannot be reconstructed") {
    AddressOrAlias.fromString("alias::sasha").left.value
    AddressOrAlias.fromString("alias:T: sasha").left.value
    AddressOrAlias.fromString("alias:T:sasha\nivanov").left.value
    AddressOrAlias.fromString("alias:T:s").left.value
    AddressOrAlias.fromString("alias:TTT:sasha").left.value

    Alias.fromString("alias:T: sasha").left.value
    Alias.fromString("alias:T:sasha\nivanov").left.value
    Alias.fromString("alias::sasha").left.value
    Alias.fromString("alias:T:s").left.value
    Alias.fromString("alias:TTT:sasha").left.value

    Alias.fromString("aliaaas:W:sasha").left.value
  }

  property("Unknown address schemes cannot be parsed") {
    AddressOrAlias.fromString("postcode:119072").left.value
  }
}
