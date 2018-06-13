package com.wavesplatform.lang

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Types.TYPE
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

  property("patternMatching") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointB => 1
        |  case pa: PointC => 2
        |}""".stripMargin
    eval[Long](sampleScript, Some(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, Some(pointBInstance)) shouldBe Right(1)
  }

  property("patternMatching with named union types") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointBC => 1
        |}""".stripMargin
    eval[Long](sampleScript, Some(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, Some(pointBInstance)) shouldBe Right(1)
  }

  property("union types have filds") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.YB
        |}""".stripMargin
    eval[Long](sampleScript, Some(pointAInstance)) shouldBe Right(3)
    eval[Long](sampleScript, Some(pointBInstance)) shouldBe Right(41)
    eval[Long](sampleScript, Some(pointCInstance)) shouldBe Right(42)
  }

  property("union types have  only common filds") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.X
        |}""".stripMargin
    eval[Long](sampleScript, Some(pointCInstance)) should produce("Compilation failed: Undefined field `X`")
  }

  property("patternMatching _") {
    val sampleScript =
      """|
         |match p {
         |  case _: PointA => 0
         |  case _: PointB  => 1
         |  case _: PointC => 2
         |}
         |
      """.stripMargin
    eval[Long](sampleScript, Some(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, Some(pointBInstance)) shouldBe Right(1)
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
    eval[Long](sampleScript, Some(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, Some(pointBInstance)) shouldBe Right(1)
  }

  property("patternMatching block") {
    val sampleScript =
      """|
         |match (let x = 1; p) {
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[Long](sampleScript, Some(pointBInstance)) shouldBe Right(1)
  }

  private def eval[T](code: String, pointInstance: Option[CaseObj] = None): Either[String, T] = {
    val untyped = Parser(code).get.value
    require(untyped.size == 1)
    val lazyVal                                     = LazyVal(EitherT.pure(pointInstance.getOrElse(null)))
    val stringToTuple: Map[String, (TYPE, LazyVal)] = Map(("p", (AorBorC, lazyVal)))
    val ctx: CTX =
      Monoid.combine(PureContext.ctx, CTX(sampleTypes, stringToTuple, Seq.empty))
    val typed = CompilerV1(ctx.compilerContext, untyped.head)
    typed.flatMap(v => EvaluatorV1[T](ctx.evaluationContext, v._1)._2)
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

  property("equals should work with Option") {
    eval[Boolean]("Some(1) == Some(1)") shouldBe Right(true)
    eval[Boolean]("Some(true) == Some(false)") shouldBe Right(false)
  }

  property("equals some lang structure") {
    eval[Boolean]("let x = (-7763390488025868909>-1171895536391400041); let v = false; (v&&true)") shouldBe Right(false)
    eval[Boolean]("let mshUmcl = (if(true) then true else true); true || mshUmcl") shouldBe Right(true)
    eval[Long]("""if(((1+-1)==-1)) then 1 else (1+1)""") shouldBe Right(2)
    eval[Boolean]("""((((if(true) then 1 else 1)==2)||((if(true)
                    |then true else true)&&(true||true)))||(if(((1>1)||(-1>=-1)))
                    |then (-1>=1) else false))""".stripMargin) shouldBe Right(true)
  }

  property("sum/mul/div/mod/fraction functions") {
    eval[Long]("(10 + 10)") shouldBe Right(20)
    eval[Long]("(10 * 10)") shouldBe Right(100)
    eval[Long]("(10 / 3)") shouldBe Right(3)
    eval[Long]("(10 % 3)") shouldBe Right(1)
    eval[Long]("fraction(9223372036854775807, 2, 4)") shouldBe Right(Long.MaxValue / 2)
    eval[Long]("fraction(9223372036854775807, 3, 2)") shouldBe Left(s"Long overflow: value `${BigInt(Long.MaxValue) * 3 / 2}` greater than 2^63-1")
    eval[Long]("fraction(-9223372036854775807, 3, 2)") shouldBe Left(s"Long overflow: value `${-BigInt(Long.MaxValue) * 3 / 2}` less than -2^63-1")
  }

  property("equals works on elements from Gens") {
    List(CONST_LONGgen, SUMgen(50), INTGen(50)).foreach(gen =>
      forAll(for {
        (expr, res) <- gen
        str         <- toString(expr)
      } yield (str, res)) {
        case (str, res) =>
          withClue(str) {
            eval[Long](str) shouldBe Right(res)
          }
    })

    forAll(for {
      (expr, res) <- BOOLgen(50)
      str         <- toString(expr)
    } yield (str, res)) {
      case (str, res) =>
        withClue(str) {
          eval[Boolean](str) shouldBe Right(res)
        }
    }
  }

  property("Extract from Some") {
    eval[Long]("extractU(SomeU(1))+1") shouldBe Right(2)
  }
}
