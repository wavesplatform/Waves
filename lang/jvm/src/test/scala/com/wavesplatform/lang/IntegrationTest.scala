package com.wavesplatform.lang

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.ctx._

class IntegrationTest extends PropSpec with PropertyChecks with Matchers with NoShrink {

  private def eval(code: String) = {
    val untyped = Parser(code).get.value
    val ctx     = Context(Map.empty, Map.empty, Map(multiplierFunction.name -> multiplierFunction))
    val typed   = TypeChecker(TypeChecker.TypeCheckerContext.fromContext(ctx), untyped)
    typed.flatMap(Evaluator(ctx, _))
  }

  property("pattern matching") {
    eval("""
           |let MULTICHARVARNAME = Some(500)
           |
           |let Z = match(MULTICHARVARNAME) {
           | case None => 8
           | case Some(B) => B + B
           | }
           |
           | get(Some(Z)) + 1
           |
      """.stripMargin) shouldBe Right(1001)

    eval("""
           |
           | let X = Some(10)
           |
           |match(X) {
           |  case None => 0
           |  case Some(V) => V + V + V + V
           |}
         """.stripMargin) shouldBe Right(40)

    eval("""
           |
           |let X = Some(10)
           |
           |match(X) {
           |  case Some(V) => V + V + V + V
           |  case None => 0
           |}
         """.stripMargin) shouldBe Right(40)
  }

  property("function call") {
    eval("MULTIPLY(3,4)".stripMargin) shouldBe Right(12)

  }
}
