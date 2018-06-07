package com.wavesplatform.helpers

import org.scalatest.matchers.{MatchResult, Matcher}

sealed trait PositionMatcher extends Matcher[(Int, Int)]

object PositionMatcher {
//
//  def expectedPosition(pm: PositionMatcher): (Int, Int) = {
//    case AnyPosition => (-1, -1)
//    case StartMatch(s) => (s, -1)
//    case EndMatch(e) => (-1, e)
//    case StartEndMatch(s, e) => (s, e)
//  }

  case object AnyPosition extends PositionMatcher {
    override def apply(left: (Int, Int)): MatchResult =
      MatchResult(
        true,
        "Any position",
        "Any position",
        IndexedSeq(left, (-1, -1))
      )
  }

  final case class StartMatch(s: Int) extends PositionMatcher {
    override def apply(left: (Int, Int)): MatchResult =
      MatchResult(
        s == left._1,
        s"Expected expression start == ${s}, but ${left._1} found",
        s"Expected expression start != ${s}",
        IndexedSeq(left, (s, -1))
      )
  }

  final case class EndMatch(e: Int) extends PositionMatcher {
    override def apply(left: (Int, Int)): MatchResult =
      MatchResult(
        e == left._2,
        s"Expected expression end == ${e}, but ${left._2} found",
        s"Expected expression end != ${e}",
        IndexedSeq(left, (-1, e))
      )
  }

  final case class StartEndMatch(s: Int, e: Int) extends PositionMatcher {
    override def apply(left: (Int, Int)): MatchResult = {
      StartMatch(s)(left) <|> EndMatch(e)(left)
    }
  }
}