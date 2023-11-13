package com.wavesplatform.lang.parser

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.getDecompilerContext
import com.wavesplatform.lang.v1.compiler.{Decompiler, TestCompiler}
import com.wavesplatform.test.PropSpec

class TupleTypeCommentsTest extends PropSpec {
  property("allowed comments between definitions of tuple types") {
    val expr = TestCompiler(V6)
      .compile(
        """
          | func f(
          |   a: (                # comment
          |                       # comment
          |     Int,              # comment
          |                       # comment
          |     String,           # comment
          |                       # comment
          |     (                 # comment
          |                       # comment
          |       List[(          # comment
          |                       # comment
          |         List[String], # comment
          |                       # comment
          |         (             # comment
          |                       # comment
          |           Address,    # comment
          |                       # comment
          |           Boolean     # comment
          |                       # comment
          |         )             # comment
          |                       # comment
          |       )],             # comment
          |                       # comment
          |       Int             # comment
          |                       # comment
          |     )                 # comment
          |   )                   # comment
          | ) = []
        """.stripMargin
      )
      .explicitGet()
    Decompiler(expr, getDecompilerContext(V6, DApp), V6).trim shouldBe "func f (a) = nil"
  }
}
