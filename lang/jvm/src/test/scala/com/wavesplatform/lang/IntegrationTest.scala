package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.ctx.impl.PureContext
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IntegrationTest extends PropSpec with PropertyChecks with Matchers with NoShrink {

  private def eval[T: TypeInfo](code: String) = {
    val untyped = Parser(code).get.value
    val ctx     = PureContext.instance
    val typed   = TypeChecker(TypeChecker.TypeCheckerContext.fromContext(ctx), untyped)
    typed.flatMap(Evaluator[T](ctx, _))
  }

  property("function call") {
    eval[Long]("10 + 2".stripMargin) shouldBe Right(12)
  }

  property("equals on byte array") {
    eval[Boolean]("base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8'".stripMargin) shouldBe Right(true)
  }
}
