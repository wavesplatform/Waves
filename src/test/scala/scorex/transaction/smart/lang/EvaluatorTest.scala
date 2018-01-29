package scorex.transaction.smart.lang

import com.wavesplatform.{NoShrink, ScriptGen}
import com.wavesplatform.state2.diffs._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.smart.lang.Evaluator.Context
import scorex.transaction.smart.lang.Terms._

class EvaluatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private def ev(c: Expr) = {
    val codec = Serde.codec
    val c2 = codec.decode(codec.encode(c).require).require.value
    Evaluator.apply(Context(0, null, Map.empty), c2)
  }

  property("successful on unused let") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        CONST_INT(3)
      )) shouldBe Right(3)
  }

  property("successful on simple get") {
    ev(
      CExpr(
        Some(LET("x", CONST_INT(3))),
        REF("x")
      )) shouldBe Right(3)

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

  property("successful on same value names in different branches") {
    ev(
      IF(EQ_INT(CONST_INT(1), CONST_INT(2)), CExpr(Some(LET("x", CONST_INT(3))), CONST_INT(500)), CExpr(Some(LET("x", CONST_INT(3))), CONST_INT(501)))
    ) shouldBe Right(501)
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
}
