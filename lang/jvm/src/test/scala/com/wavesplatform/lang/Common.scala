package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.scalacheck.Shrink
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Left, Right, Try}

object Common {

  def ev[T](context: EvaluationContext = PureContext.evalContext, expr: EXPR): (EvaluationContext, Either[ExecutionError, T]) =
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

  val multiplierFunction: PredefFunction = PredefFunction("MULTIPLY", 1, 512, List("x1" -> LONG, "x2" -> LONG), LONG) {
    case (x1: Long) :: (x2: Long) :: Nil => Try(x1 * x2).toEither.left.map(_.toString)
    case _                               => ??? // suppress pattern match warning
  }

  val pointTypeA = CaseType("PointA", List("X"  -> LONG, "YA" -> LONG))
  val pointTypeB = CaseType("PointB", List("X"  -> LONG, "YB" -> LONG))
  val pointTypeC = CaseType("PointC", List("YB" -> LONG))

  val AorB    = UNION.of(pointTypeA, pointTypeB)
  val AorBorC = UNION.of(pointTypeA, pointTypeB, pointTypeC)
  val BorC    = UNION.of(pointTypeB, pointTypeC)

  val pointAInstance = CaseObj(pointTypeA.typeRef, Map("X"  -> 3L, "YA" -> 40L))
  val pointBInstance = CaseObj(pointTypeB.typeRef, Map("X"  -> 3L, "YB" -> 41L))
  val pointCInstance = CaseObj(pointTypeC.typeRef, Map("YB" -> 42L))

  val sampleTypes: Seq[DefinedType] = Seq(pointTypeA, pointTypeB, pointTypeC) ++ Seq(UnionType("PointAB", AorB.l), UnionType("PointBC", BorC.l))

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(Map("p" -> LazyVal(EitherT.pure(instance))), Seq.empty)
}
