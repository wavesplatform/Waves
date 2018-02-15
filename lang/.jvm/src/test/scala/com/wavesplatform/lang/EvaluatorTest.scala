package com.wavesplatform.lang

import com.wavesplatform.lang.Evaluator.{Context, Defs}
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector
import Implicits._

class EvaluatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev(predefTypes: Map[String, CUSTOMTYPE] = Map.empty, defs: Defs = Map.empty, expr: Expr): Either[_, _] =
    Evaluator.apply(Context(predefTypes, defs), expr)

  private def simpleDeclarationAndUsage(i: Int) = Block(Some(LET("x", CONST_INT(i))), REF("x"))

  property("successful on very deep expressions (stack overflow check)") {
    val term = (1 to 100000).foldLeft[Terms.Expr](CONST_INT(0))((acc, _) => SUM(acc, CONST_INT(1)))
    ev(expr = term) shouldBe Right(100000)
  }

  property("successful on unused let") {
    ev(
      expr = Block(
        Some(LET("x", CONST_INT(3))),
        CONST_INT(3)
      )) shouldBe Right(3)
  }

  property("successful on x = y") {
    ev(
      expr = Block(Some(LET("x", CONST_INT(3))),
                   Block(
                     Some(LET("y", REF("x"))),
                     SUM(REF("x"), REF("y"))
                   ))) shouldBe Right(6)
  }

  property("successful on simple get") {
    ev(expr = simpleDeclarationAndUsage(3)) shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev(
      expr = Block(
        Some(LET("x", CONST_INT(3))),
        EQ(REF("x"), CONST_INT(2))
      )) shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev(
      expr = Block(
        Some(LET("x", CONST_INT(3))),
        Block(Some(LET("y", CONST_INT(3))), EQ(REF("x"), REF("y")))
      )) shouldBe Right(true)
  }

  property("successful on multiple lets with expression") {
    ev(
      expr = Block(
        Some(LET("x", CONST_INT(3))),
        Block(Some(LET("y", SUM(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), REF("y")))
      )) shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev(expr = IF(EQ(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), CONST_INT(4))) shouldBe Right(4)
  }

  property("successful on same value names in different branches") {
    ev(expr = IF(EQ(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4))) shouldBe Right(4)
  }

  property("fails if override") {
    ev(
      expr = Block(
        Some(LET("x", CONST_INT(3))),
        Block(Some(LET("x", SUM(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), CONST_INT(1)))
      )) should produce("already defined")
  }

  property("fails if types do not match") {
    ev(
      expr = Block(
        Some(LET("x", CONST_INT(3))),
        Block(Some(LET("y", EQ(CONST_INT(3), CONST_INT(0)))), EQ(REF("x"), REF("y")))
      )) should produce("Typecheck failed")
  }

  property("fails if definition not found") {
    ev(expr = SUM(REF("x"), CONST_INT(2))) should produce("A definition of 'x' is not found")
  }

  property("fails if 'IF' branches lead to different types") {
    ev(expr = IF(EQ(CONST_INT(1), CONST_INT(2)), CONST_INT(0), CONST_BYTEVECTOR(ByteVector.empty))) should produce("Typecheck failed")
  }

  property("Typechecking") {
    ev(expr = EQ(CONST_INT(2), CONST_INT(2))) shouldBe Right(true)
  }

  property("successful Typechecking Some") {
    ev(expr = EQ(SOME(CONST_INT(2)), SOME(CONST_INT(2)))) shouldBe Right(true)
  }

  property("successful Typechecking Option") {
    ev(expr = EQ(SOME(CONST_INT(2)), NONE)) shouldBe Right(false)
    ev(expr = EQ(NONE, SOME(CONST_INT(2)))) shouldBe Right(false)
  }

  property("successful nested Typechecking Option") {
    ev(expr = EQ(SOME(SOME(SOME(CONST_INT(2)))), NONE)) shouldBe Right(false)
    ev(expr = EQ(SOME(NONE), SOME(SOME(CONST_INT(2))))) shouldBe Right(false)
  }

  property("fails if nested Typechecking Option finds mismatch") {
    ev(expr = EQ(SOME(SOME(FALSE)), SOME(SOME(CONST_INT(2))))) should produce("Typecheck failed")
  }

  property("successful GET/IS_DEFINED") {
    ev(expr = IS_DEFINED(NONE)) shouldBe Right(false)
    ev(expr = IS_DEFINED(SOME(CONST_INT(1)))) shouldBe Right(true)
    ev(expr = GET(SOME(CONST_INT(1)))) shouldBe Right(1)
  }

  property("successful resolve strongest type") {
    ev(expr = GET(IF(TRUE, SOME(CONST_INT(3)), SOME(CONST_INT(2))))) shouldBe Right(3)
    ev(expr = GET(IF(TRUE, SOME(CONST_INT(3)), NONE))) shouldBe Right(3)
    ev(expr = SUM(CONST_INT(1), GET(IF(TRUE, SOME(CONST_INT(3)), NONE)))) shouldBe Right(4)
  }

  property("custom type field access") {
    val pointType     = CUSTOMTYPE("Point", List("X" -> INT, "Y" -> INT))
    val pointInstance = OBJECT(Map("X" -> LazyVal(INT)(Coeval(3)), "Y" -> LazyVal(INT)(Coeval(4))))
    ev(
      predefTypes = Map(pointType.name -> pointType),
      defs = Map(("p", (TYPEREF(pointType.name), pointInstance))),
      expr = SUM(GETTER(REF("p"), "X"), CONST_INT(2))
    ) shouldBe Right(5)

  }
}
