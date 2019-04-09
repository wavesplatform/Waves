package com.wavesplatform.account

import com.wavesplatform.common.utils.EitherExt2
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AccountOrAliasTests extends PropSpec with PropertyChecks with Matchers {

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
  property("Alias cannot be from other network") {
    AddressOrAlias.fromString("alias:Q:sasha") shouldBe 'left
  }

  property("Malformed aliases cannot be reconstructed") {
    AddressOrAlias.fromString("alias::sasha") shouldBe 'left
    AddressOrAlias.fromString("alias:T: sasha") shouldBe 'left
    AddressOrAlias.fromString("alias:T:sasha\nivanov") shouldBe 'left
    AddressOrAlias.fromString("alias:T:s") shouldBe 'left
    AddressOrAlias.fromString("alias:TTT:sasha") shouldBe 'left

    Alias.fromString("alias:T: sasha") shouldBe 'left
    Alias.fromString("alias:T:sasha\nivanov") shouldBe 'left
    Alias.fromString("alias::sasha") shouldBe 'left
    Alias.fromString("alias:T:s") shouldBe 'left
    Alias.fromString("alias:TTT:sasha") shouldBe 'left

    Alias.fromString("aliaaas:W:sasha") shouldBe 'left
  }

  property("Unknown address schemes cannot be parsed") {
    AddressOrAlias.fromString("postcode:119072") shouldBe 'left
  }
}
