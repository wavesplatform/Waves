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
    eval[Long]("10 + 2") shouldBe Right(12)
  }

  property("equals works on primitive types") {
    eval[Boolean]("base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8' == base58'3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8'") shouldBe Right(true)
    eval[Boolean]("1 == 2") shouldBe Right(false)
    eval[Boolean]("3 == 3") shouldBe Right(true)
    eval[Boolean]("false == false") shouldBe Right(true)
    eval[Boolean]("true == false") shouldBe Right(false)
    eval[Boolean]("true == true") shouldBe Right(true)
    eval[Boolean]("""   "x" == "x"     """) shouldBe Right(true)
    eval[Boolean]("""   "x" == "y"     """) shouldBe Right(false)
  }

  property("equals shouldn't compile on option") {
    eval[Boolean]("Some(1) == Some(2)") should produce("ERROR")
  }
}
