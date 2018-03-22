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

  property("function call") {
    eval("MULTIPLY(3,4)".stripMargin) shouldBe Right(12)
  }

  property("equals on byte array") {
    eval("base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8'".stripMargin) shouldBe Right(true)
  }
}
