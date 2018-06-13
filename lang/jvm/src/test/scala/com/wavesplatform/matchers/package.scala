package com.wavesplatform

import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.matchers.ExprMatcher._
import org.scalatest.matchers.{BeMatcher, MatchResult}

import scala.reflect.runtime.universe._

package object matchers {
  implicit class MatchResultExt(m: MatchResult) {
    def <|>[B](other: MatchResult): MatchResult =
      if (m.matches) other
      else m
  }

  def wrongTypeMatchResult[L: TypeTag, R: TypeTag]: MatchResult =
    MatchResult(
      false,
      s"Expected ${typeOf[R]}, but get ${typeOf[L]}",
      s"Unexpected ${typeOf[R]}",
      IndexedSeq("left", "right")
    )

  def positionedMatcher[A](start: Int, end: Int, pos: PositionMatcher, matcher: BeMatcher[A])(a: A): MatchResult =
    pos((start, end)) <|> matcher(a)

  def any[T]: BeMatcher[T]                       = (_: T) => MatchResult(true, "", "")
  def any(pos: PositionMatcher): BeMatcher[EXPR] = (e: EXPR) => pos((e.start, e.end)) <|> any(e)

  val anyExpr: ExprMatcher = AnyExpr
}
