package com.wavesplatform.lang

import com.wavesplatform.lang.ctx.PredefFunction
import org.scalacheck.Shrink
import org.scalatest.matchers.{Matcher, MatchResult}

import scala.util.{Left, Right, Try}

object Common {

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

  val multiplierFunction: PredefFunction = PredefFunction("MULTIPLY", Terms.INT, List(("x1", Terms.INT), ("x2", Terms.INT))) {
    case (x1: Int) :: (x2: Int) :: Nil => Try(x1 * x2).toEither.left.map(_.toString)
    case _                             => ??? // suppress pattern match warning
  }
}
