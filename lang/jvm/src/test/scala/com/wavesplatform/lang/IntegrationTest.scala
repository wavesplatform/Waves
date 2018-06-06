package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class IntegrationTest extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink {

  def withUnion(p: CaseObj): EvaluationContext = Monoid.combine(PureContext.evalContext, sampleUnionContext(p))

  property("patternMatching") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointB => 1
        |  case pa: PointC => 2
        |}""".stripMargin
    eval[Long](sampleScript, withUnion(pointAInstance)) shouldBe Right(0)
    eval[Long](sampleScript, withUnion(pointBInstance)) shouldBe Right(1)
  }

  property("patternMatching with named union types") {
    val sampleScript =
      """match p {
        |  case pa: PointA => 0
        |  case pa: PointBC => 1
        |}""".stripMargin
    eval[Long](sampleScript, withUnion(pointAInstance), { c =>
      Map("PointBC" -> UnionType("PointBC", List(pointTypeB, pointTypeC).map(_.typeRef)))
    }) shouldBe Right(0)
    eval[Long](sampleScript, withUnion(pointBInstance), { c =>
      Map("PointBC" -> UnionType("PointBC", List(pointTypeB, pointTypeC).map(_.typeRef)))
    }) shouldBe Right(1)
  }

  property("union types have filds") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.YB
        |}""".stripMargin
    eval[Long](sampleScript, withUnion(pointAInstance), { c =>
      Map("PointBC" -> UnionType("PointBC", List(pointTypeB, pointTypeC).map(_.typeRef)))
    }) shouldBe Right(3)
    eval[Long](sampleScript, withUnion(pointBInstance), { c =>
      Map("PointBC" -> UnionType("PointBC", List(pointTypeB, pointTypeC).map(_.typeRef)))
    }) shouldBe Right(41)
    eval[Long](sampleScript, withUnion(pointCInstance), { c =>
      Map("PointBC" -> UnionType("PointBC", List(pointTypeB, pointTypeC).map(_.typeRef)))
    }) shouldBe Right(42)
  }

  property("union types have  only common filds") {
    val sampleScript =
      """match p {
        |  case pa: PointA => pa.X
        |  case pb: PointBC => pb.X
        |}""".stripMargin
    eval[Long](sampleScript, withUnion(pointCInstance), { c =>
      Map("PointBC" -> UnionType("PointBC", List(pointTypeB, pointTypeC).map(_.typeRef)))
    }) should produce("Compilation failed: Undefined field `X`")
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

  property("patternMatching block") {
    val sampleScript =
      """|
         |match (let x = 1; p) {
         |  case _  => 1
         |}
         |
      """.stripMargin
    eval[Long](sampleScript, withUnion(pointBInstance)) shouldBe Right(1)
  }

  private def eval[T](code: String, ctx: EvaluationContext = PureContext.evalContext, genTypes: CompilerContext => Map[String, DefinedType] = { _ =>
    Map.empty
  }): Either[String, T] = {
    val untyped = Parser(code).get.value
    require(untyped.size == 1)
//    val cctx = CompilerContext.fromEvaluationContext(
//      ctx,
//      sampleTypes.map(v => v.name -> v).toMap,
//      Map(PureContext.errRef -> PureContext.predefVars(PureContext.errRef), // need for match fails handling
//          "p"                -> AorBorC)
//    )
//    val types = genTypes(cctx)
//    val typed = CompilerV1(cctx.copy(predefTypes = types ++ cctx.predefTypes), untyped.head)
//    typed.flatMap(v => EvaluatorV1[T](ctx, v._1)._2)
    ???
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

}
