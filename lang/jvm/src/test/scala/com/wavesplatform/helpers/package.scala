package com.wavesplatform

import org.scalatest.matchers.{MatchResult, Matcher}

import scala.reflect.runtime.universe._

package object helpers {
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

  def positionedMatcher[A](start: Int, end: Int, pos: PositionMatcher, matcher: Matcher[A])(a: A): MatchResult =
    pos((start, end)) <|> matcher(a)

  def __[A]: Matcher[A] = new Matcher[A] {
    override def apply(left: A): MatchResult = MatchResult(true, "", "")
  }
}
