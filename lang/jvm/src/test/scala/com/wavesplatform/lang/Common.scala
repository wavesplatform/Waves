package com.wavesplatform.lang

import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.ctx.PredefFunction
import org.scalacheck.Shrink
import org.scalatest.matchers.{MatchResult, Matcher}

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

  val multiplierFunction: PredefFunction = PredefFunction("MULTIPLY", 1, Terms.LONG, List(("x1", Terms.LONG), ("x2", Terms.LONG))) {
    case (x1: Long) :: (x2: Long) :: Nil => Try(x1 * x2).toEither.left.map(_.toString)
    case _                               => ??? // suppress pattern match warning
  }
}
