package com.wavesplatform.account

import com.wavesplatform.test.PropSpec

class AliasSpecification extends PropSpec {

  property("Correct alias should be valid") {
    forAll(validAliasStringGen) { s =>
      Alias.create(s) should beRight
    }
  }

  property("Incorrect alias should be invalid") {
    forAll(invalidAliasStringGen) { s =>
      Alias.create(s) should beLeft
    }
  }
}
