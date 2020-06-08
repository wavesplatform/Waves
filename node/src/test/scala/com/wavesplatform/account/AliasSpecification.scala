package com.wavesplatform.account

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class AliasSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with EitherValues {

  property("Correct alias should be valid") {
    forAll(validAliasStringGen) { s =>
      Alias.create(s).explicitGet()
    }
  }

  property("Incorrect alias should be invalid") {
    forAll(invalidAliasStringGen) { s =>
      Alias.create(s).left.value
    }
  }
}
