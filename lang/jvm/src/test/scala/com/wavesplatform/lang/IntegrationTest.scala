package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.{EvaluatorV1, Parser, TypeChecker}
import com.wavesplatform.lang.v1.ctx.impl.PureContext
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IntegrationTest extends PropSpec with PropertyChecks with Matchers with NoShrink {

  private def eval[T: TypeInfo](code: String) = {
    val untyped = Parser(code).get.value
    val ctx     = PureContext.instance
    val typed   = TypeChecker(TypeChecker.TypeCheckerContext.fromContext(ctx), untyped)
    typed.flatMap(EvaluatorV1[T](ctx, _))
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
    eval[Boolean]("Some(1) == Some(2)") should produce("Can't find a function '=='")
  }

  property("equals some lang structure") {
    eval[Boolean]("let x = (-7763390488025868909>-1171895536391400041) let v = false (v&&true)") shouldBe Right(false)
    eval[Boolean]("let mshUmcl = (if(true) then true else true) true || mshUmcl") shouldBe Right(true)
    eval[Long]("""if(((1+-1)==-1)) then 1 else (1+1)""") shouldBe Right(2)
    eval[Boolean]("""((((if(true) then 1 else 1)==2)||((if(true)
                    |then true else true)&&(true||true)))||(if(((1>1)||(-1>=-1)))
                    |then (-1>=1) else false))""".stripMargin) shouldBe Right(true)
  }
}
