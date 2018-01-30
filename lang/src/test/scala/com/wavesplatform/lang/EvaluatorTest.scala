package com.wavesplatform.lang

import com.wavesplatform.lang.Evaluator.Context
import com.wavesplatform.lang.Terms._
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector

class EvaluatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev(c: Expr) = {
    val codec = Serde.codec
    val c2    = codec.decode(codec.encode(c).require).require.value
    Evaluator.apply(Context(new HeightDomain(1), Map.empty), c2)
  }

  private def simpleDeclarationAndUsage(i: Int) = CExpr(Some(LET("x", CONST_INT(i))), REF("x"))

  property("successful on unused let") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        CONST_INT(3)
      )) shouldBe Right(3)
  }

  property("successful on x = y") {
    ev(
      CExpr(Some(LET("x", CONST_INT(3))),
        CExpr(
          Some(LET("y", REF("x"))),
          SUM(REF("x"), REF("y"))
        ))) shouldBe Right(6)
  }



  property("successful on simple get") {
    ev(simpleDeclarationAndUsage(3)) shouldBe Right(3)
  }

  property("successful on get used further in expr") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        EQ_INT(REF("x"), CONST_INT(2))
      )) shouldBe Right(false)
  }

  property("successful on multiple lets") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        CExpr(Some(LET("y", CONST_INT(3))), EQ_INT(REF("x"), REF("y")))
      )) shouldBe Right(true)
  }

  property("successful on multiple lets with expression") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        CExpr(Some(LET("y", SUM(CONST_INT(3), CONST_INT(0)))), EQ_INT(REF("x"), REF("y")))
      )) shouldBe Right(true)
  }

  property("successful on deep type resolution") {
    ev(
      IF(EQ_INT(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), CONST_INT(4))
    ) shouldBe Right(4)
  }

  property("successful on same value names in different branches") {
    ev(
      IF(EQ_INT(CONST_INT(1), CONST_INT(2)), simpleDeclarationAndUsage(3), simpleDeclarationAndUsage(4))
    ) shouldBe Right(4)
  }

  property("fails if override") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        CExpr(Some(LET("x", SUM(CONST_INT(3), CONST_INT(0)))), EQ_INT(REF("x"), REF("y")))
      )) should produce("already defined")
  }

  property("fails if types do not match") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        CExpr(Some(LET("y", EQ_INT(CONST_INT(3), CONST_INT(0)))), EQ_INT(REF("x"), REF("y")))
      )) should produce("cannot be cast")
  }

  property("fails if definition not found") {
    ev(EQ_INT(REF("x"), CONST_INT(2))) should produce("Definition 'x' not found")
  }

  property("fails if 'IF' branches lead to different types") {
    ev(
      IF(EQ_INT(CONST_INT(1), CONST_INT(2)), CONST_INT(0), CONST_BYTEVECTOR(ByteVector.empty))
    ) should produce("Typecheck failed: RType")
  }
}
