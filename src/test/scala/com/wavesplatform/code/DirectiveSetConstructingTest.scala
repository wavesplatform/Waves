package com.wavesplatform.code

import com.wavesplatform.lang.Common.produce
import com.wavesplatform.lang.ContentType.{Contract, Expression}
import com.wavesplatform.lang.ScriptType.{Account, Asset}
import com.wavesplatform.lang.StdLibVersion.{V1, V2, V3}
import com.wavesplatform.lang.utils.DirectiveSet
import org.scalatest.{Matchers, PropSpec}

class DirectiveSetConstructingTest extends PropSpec with Matchers {
  property("DirectiveSet should be successfully constructed with (V3, Account, Contract) params") {
    DirectiveSet(V3, Account, Contract) shouldBe DirectiveSet(V3, Account, Contract)
  }

  property("DirectiveSet should be successfully constructed with (<any>, <any>, Expression) params") {
    DirectiveSet(V1, Account, Expression) shouldBe DirectiveSet(V1, Account, Expression)
    DirectiveSet(V2,   Asset, Expression) shouldBe DirectiveSet(V2,   Asset, Expression)
    DirectiveSet(V3,   Asset, Expression) shouldBe DirectiveSet(V3,   Asset, Expression)
  }

  property("DirectiveSet should not be constructed with wrong param set") {
    DirectiveSet(V1, Account, Contract) should produce("Inconsistent set of directives")
    DirectiveSet(V2, Account, Contract) should produce("Inconsistent set of directives")
    DirectiveSet(V3,   Asset, Contract) should produce("Inconsistent set of directives")
  }
}
