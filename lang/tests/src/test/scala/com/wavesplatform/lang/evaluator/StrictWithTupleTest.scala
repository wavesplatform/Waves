package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.{StdLibVersion, V4}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN

class StrictWithTupleTest extends EvaluatorSpec {
  private implicit val version: StdLibVersion = V4

  property("strict with tuple") {
    eval(
      """
        | strict (str, i, cond, bytes) = ("12345", 12345, true, base58'')
        | str.parseInt() == i && cond && bytes.size() == 0
      """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }
}
