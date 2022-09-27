package com.wavesplatform.lang.evaluator
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG

class MultiLineOperatorTest extends EvaluatorSpec {
  property("declaration with '-' after number inside let definition") {
    eval(
      s"""
         |  let a = {
         |    let b = 1000
         |    -1000
         |  }
         |  a
      """.stripMargin
    )(V6).explicitGet() shouldBe CONST_LONG(-1000)
  }

  property("declaration with '+' after number inside let definition") {
    eval(
      s"""
         |  let a = {
         |    let b = 1000
         |    +1000
         |  }
         |  a
      """.stripMargin
    )(V6).explicitGet() shouldBe CONST_LONG(1000)
  }

  property("multiline multiplying (default behaviour for binary operator)") {
    eval(
      s"""
         |  let a = {
         |    let b = 1000
         |    * 2
         |    * 3
         |    b
         |  }
         |  a
      """.stripMargin
    )(V6).explicitGet() shouldBe CONST_LONG(6000)
  }

  property("'+' and '-' with whitespaces") {
    eval(
      s"""
         |  let a = {
         |    let b = 1 + 1
         |    let c = 1 - 1
         |    b + c
         |  }
         |  a
      """.stripMargin
    )(V6).explicitGet() shouldBe CONST_LONG(2)
  }
}
