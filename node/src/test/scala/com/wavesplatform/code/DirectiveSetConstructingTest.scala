package com.wavesplatform.code

import com.wavesplatform.common.state.diffs.ProduceError.produce
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import org.scalatest.{Matchers, PropSpec}

class DirectiveSetConstructingTest extends PropSpec with Matchers {
  property("DirectiveSet should be successfully constructed with (V3, Account, Contract) params") {
    DirectiveSet(V3, Account, DApp) shouldBe DirectiveSet(V3, Account, DApp)
  }

  property("DirectiveSet should be successfully constructed with (<any>, <any>, Expression) params") {
    DirectiveSet(V1, Account, Expression) shouldBe DirectiveSet(V1, Account, Expression)
    DirectiveSet(V2, Asset, Expression) shouldBe DirectiveSet(V2, Asset, Expression)
    DirectiveSet(V3, Asset, Expression) shouldBe DirectiveSet(V3, Asset, Expression)
  }

  property("DirectiveSet should not be constructed with wrong param set") {
    DirectiveSet(V1, Account, DApp) should produce("Inconsistent set of directives")
    DirectiveSet(V2, Account, DApp) should produce("Inconsistent set of directives")
    DirectiveSet(V3, Asset, DApp) should produce("Inconsistent set of directives")
  }
}
