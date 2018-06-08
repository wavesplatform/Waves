package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers.ExprMatcher.ConstLong
import com.wavesplatform.matchers.PositionMatcher.{AnyPosition, StartEndMatch}
import com.wavesplatform.matchers.{ExprMatcher, PositionMatcher}

trait LongFunctions {
  def constLong(start: Int, end: Int, value: Long): ExprMatcher = ConstLong(StartEndMatch(start, end), value)
  def constLong(pos: PositionMatcher, value: Long): ExprMatcher = ConstLong(pos, value)
  def constLong(value: Long): ExprMatcher                       = ConstLong(AnyPosition, value)
}
