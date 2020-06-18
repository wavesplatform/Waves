package com.wavesplatform

import org.scalatest.matchers.{MatchResult, Matcher}

trait EitherMatchers {
  import EitherMatchers._
  def beRight: Matcher[Either[Any, Any]] = BeRight
  def beLeft: Matcher[Either[Any, Any]]  = BeLeft
}

object EitherMatchers extends EitherMatchers {
  private[EitherMatchers] abstract class EitherMatcher(side: String) extends Matcher[Either[Any, Any]] {
    protected def matches(valueToCheck: Either[Any, Any]): Boolean
    override def apply(valueToCheck: Either[Any, Any]): MatchResult = MatchResult(
      matches(valueToCheck),
      s"$valueToCheck was not $side",
      s"$valueToCheck was $side"
    )
  }

  private[EitherMatchers] object BeRight extends EitherMatcher("right") {
    override protected def matches(valueToCheck: Either[Any, Any]): Boolean = valueToCheck.isRight
  }

  private[EitherMatchers] object BeLeft extends EitherMatcher("left") {
    override protected def matches(valueToCheck: Either[Any, Any]): Boolean = valueToCheck.isLeft
  }
}
