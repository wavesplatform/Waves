package com.wavesplatform.lang

import cats.data.{EitherT, NonEmptyList}
import cats.syntax.semigroup._
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class EvaluatorV1PatMatTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  val pointTypeA = PredefCaseType("PointA", List("X" -> LONG, "YA" -> LONG))
  val pointTypeB = PredefCaseType("PointB", List("X" -> LONG, "YB" -> LONG))

  val AorB = UNION(NonEmptyList.of(CASETYPEREF(pointTypeA.typeRef.name), CASETYPEREF(pointTypeB.typeRef.name)))

  val pointAInstance = CaseObj(pointTypeA.typeRef, Map("X" -> Val(LONG)(3), "YA" -> Val(LONG)(40)))
  val pointBInstance = CaseObj(pointTypeB.typeRef, Map("X" -> Val(LONG)(3), "YB" -> Val(LONG)(41)))

  private def context(instance: CaseObj) =
    PureContext.instance |+| EvaluationContext.build(Seq.empty,
                                                     Seq(pointTypeA, pointTypeB),
                                                     Map("p" -> LazyVal(AorB)(EitherT.pure(instance))),
                                                     Seq.empty)

  property("case custom type field access") {
    ev[Long](
      context = context(pointAInstance),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", TYPEREF("PointA")), "X", LONG), CONST_LONG(2)), LONG)
    ) shouldBe Right(5)
  }

  property("case custom type field access over union") {
    def testAccess(instance: CaseObj, field: String) = ev[Long](
      context = context(instance),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p", AorB), field, LONG), CONST_LONG(2)), LONG)
    )

    testAccess(pointAInstance, "X") shouldBe Right(5)
    testAccess(pointBInstance, "X") shouldBe Right(5)
    testAccess(pointAInstance, "YA") shouldBe Right(42)
    testAccess(pointBInstance, "YB") shouldBe Right(43)
    testAccess(pointAInstance, "YB") should produce("field 'YB' not found")
  }

  property("evaluate according to pattern") {
    val expr = MATCH(REF("p", AorB),
                     List(
                       MATCH_CASE(List(CASETYPEREF("PointA")), CONST_LONG(0)),
                       MATCH_CASE(List(CASETYPEREF("PointB")), CONST_LONG(1))
                     ),
                     LONG)
    ev[Long](context(pointAInstance), expr) shouldBe Right(0)
    ev[Long](context(pointBInstance), expr) shouldBe Right(1)
  }

}
