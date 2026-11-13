package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.{StdLibVersion, V1, V3, V4, V5, V6}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
import com.wavesplatform.lang.v1.compiler.Types.LONG
import com.wavesplatform.lang.v1.evaluator.ctx.UserFunction
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.test.PropSpec

class FunctionCostTest extends PropSpec {
  private val function = UserFunction[Environment]("f", Map[StdLibVersion, Long](V3 -> 10L, V5 -> 20L), LONG)(CONST_LONG(1))

  property("a version takes its own cost, or the cost of the latest version before it") {
    function.costByLibVersion(V3) shouldBe 10
    function.costByLibVersion(V4) shouldBe 10
    function.costByLibVersion(V5) shouldBe 20
    function.costByLibVersion(V6) shouldBe 20
  }

  property("a version before every listed one has no cost") {
    (the[IllegalArgumentException] thrownBy function.costByLibVersion(V1)).getMessage should startWith("requirement failed: f ")
  }
}
