package scorex.transaction.smart.lang

import com.wavesplatform.{NoShrink, ScriptGen}
import com.wavesplatform.state2.diffs._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.smart.lang.Evaluator.Context
import scorex.transaction.smart.lang.Terms._

class EvaluatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  def ev(c: Term) = Evaluator.apply(Context(0, null, Map.empty), c)

  property("simple let") {
    ev(
      COMPOSITE(
        Some(LET("x", CONST_INT(3))),
        EQ_INT(REF("x"), CONST_INT(3))
      )) shouldBe Right(true)

    ev(
      COMPOSITE(
        Some(LET("x", CONST_INT(3))),
        EQ_INT(REF("x"), CONST_INT(2))
      )) shouldBe Right(false)
  }

  property("multiple lets") {
    ev(
      COMPOSITE(
        Some(LET("x", CONST_INT(3))),
        COMPOSITE(Some(LET("y", CONST_INT(3))), EQ_INT(REF("x"), REF("y")))
      )) shouldBe Right(true)
  }

  property("multiple lets with expression") {
    ev(
      COMPOSITE(
        Some(LET("x", CONST_INT(3))),
        COMPOSITE(Some(LET("y", SUM(CONST_INT(3), CONST_INT(0)))), EQ_INT(REF("x"), REF("y")))
      )) shouldBe Right(true)
  }

  property("fails if types do not match") {
    ev(
      COMPOSITE(
        Some(LET("x", CONST_INT(3))),
        COMPOSITE(Some(LET("y", EQ_INT(CONST_INT(3), CONST_INT(0)))), EQ_INT(REF("x"), REF("y")))
      )) should produce("cannot be cast")
  }

  property("fails if definition not found") {
    ev(EQ_INT(REF("x"), CONST_INT(2))) should produce("Definition 'x' not found")
  }
}
