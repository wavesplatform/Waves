package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers.PositionMatcher
import com.wavesplatform.matchers.PositionMatcher.{AnyPosition, EndMatch, StartEndMatch, StartMatch}

trait PosMatcherFunctions {
  val anyPos                                     = AnyPosition
  def startsFrom(s: Int): PositionMatcher        = StartMatch(s)
  def endsAt(e: Int): PositionMatcher            = EndMatch(e)
  def pos(start: Int, end: Int): PositionMatcher = StartEndMatch(start, end)
}
