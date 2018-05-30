package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CASETYPEREF, EXPR, LONG, UNION}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx._
import org.scalacheck.Shrink
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right, Try}

object Common {

  def ev[T: TypeInfo](context: EvaluationContext = PureContext.instance, expr: EXPR): (EvaluationContext, Either[ExecutionError, T]) =
    EvaluatorV1[T](context, expr)

  trait NoShrink {
    implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  }

  class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
    override def apply(ei: Either[_, _]): MatchResult = {
      ei match {
        case r @ Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
        case l @ Left(_) =>
          MatchResult(matches = l.toString contains errorMessage,
                      "expecting Left(...{0}...) but got {1}",
                      "got expected error",
                      IndexedSeq(errorMessage, l))
      }
    }
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: PredefFunction = PredefFunction("MULTIPLY", 1, Terms.LONG, List(("x1", Terms.LONG), ("x2", Terms.LONG))) {
    case (x1: Long) :: (x2: Long) :: Nil => Try(x1 * x2).toEither.left.map(_.toString)
    case _                               => ??? // suppress pattern match warning
  }

  val pointTypeA = PredefCaseType("PointA", List("X" -> LONG, "YA" -> LONG))
  val pointTypeB = PredefCaseType("PointB", List("X" -> LONG, "YB" -> LONG))

  val AorB = UNION(List(CASETYPEREF(pointTypeA.typeRef.name), CASETYPEREF(pointTypeB.typeRef.name)))

  val pointAInstance = CaseObj(pointTypeA.typeRef, Map("X" -> Val(LONG)(3), "YA" -> Val(LONG)(40)))
  val pointBInstance = CaseObj(pointTypeB.typeRef, Map("X" -> Val(LONG)(3), "YB" -> Val(LONG)(41)))

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(Seq.empty, Seq(pointTypeA, pointTypeB), Map("p" -> LazyVal(AorB)(EitherT.pure(instance))), Seq.empty)
}
