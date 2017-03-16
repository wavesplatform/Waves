package scorex.account

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class AccountOrAliasTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("Account should get parsed correctly") {
    AccountOrAlias.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").right.get shouldBe an[Account]
    AccountOrAlias.fromString("address:3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").right.get shouldBe an[Account]

    Account.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").right.get shouldBe an[Account]
    Account.fromString("address:3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").right.get shouldBe an[Account]
  }

  property("Alias should get parsed correctly") {
    val alias = AccountOrAlias.fromString("alias:T:sasha").right.get.asInstanceOf[Alias]
    alias.name shouldBe "sasha"
    alias.networkByte shouldBe 'T'

    val alias2 = Alias.fromString("alias:T:sasha").right.get
    alias2.name shouldBe "sasha"
    alias2.networkByte shouldBe 'T'

  }
  property("Alias cannot be from other network") {
    AccountOrAlias.fromString("alias:Q:sasha") shouldBe 'left
  }

  property("Malformed aliases cannot be reconstructed") {
    AccountOrAlias.fromString("alias::sasha") shouldBe 'left
    AccountOrAlias.fromString("alias:W:s") shouldBe 'left
    AccountOrAlias.fromString("alias:WWW:sasha") shouldBe 'left

    Alias.fromString("alias::sasha") shouldBe 'left
    Alias.fromString("alias:W:s") shouldBe 'left
    Alias.fromString("alias:WWW:sasha") shouldBe 'left

    Alias.fromString("aliaaas:W:sasha") shouldBe 'left
  }

  property("Unknown address schemes cannot be parsed") {
    AccountOrAlias.fromString("postcode:119072") shouldBe 'left
  }
}
