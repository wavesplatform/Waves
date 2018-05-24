package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseObj, EvaluationContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

  def withUnion(p: CaseObj): EvaluationContext = Monoid.combine(PureContext.instance, sampleUnionContext(p))

  property("patternMatching") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pb: PointB => 1
        |}""".stripMargin
    eval[Long](sampleScript, withUnion(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, withUnion(pointBInstance)) shouldBe Right(1)
  }

  property("patternMatching _") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _: PointB  => 1
         |}
         |
      """.stripMargin
    eval[Long](sampleScript, withUnion(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, withUnion(pointBInstance)) shouldBe Right(1)
  }

  property("patternMatching any type") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[Long](sampleScript, withUnion(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, withUnion(pointBInstance)) shouldBe Right(1)
  }

  private def eval[T: TypeInfo](code: String, ctx: EvaluationContext = PureContext.instance) = {
    val untyped = Parser(code).get.value
    require(untyped.size == 1)
    val typed = CompilerV1(CompilerContext.fromEvaluationContext(ctx), untyped.head)
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

  property("equals works on elements from Gens") {
    List(CONST_LONGgen, SUMgen(50), INTGen(50)).foreach(gen =>
      forAll(for {
        (expr, res) <- gen
        str         <- toString(expr)
      } yield (str, res)) {
        case (str, res) =>
          eval[Long](str) shouldBe Right(res)
    })

    forAll(for {
      (expr, res) <- BOOLgen(50)
      str         <- toString(expr)
    } yield (str, res)) {
      case (str, res) =>
        eval[Boolean](str) shouldBe Right(res)
    }
  }

}
