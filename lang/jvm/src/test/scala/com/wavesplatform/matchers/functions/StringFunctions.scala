package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers.ExprMatcher.{ConstString, PartInvalid, PartMatcher}
import com.wavesplatform.matchers.PositionMatcher.{AnyPosition, StartEndMatch}
import com.wavesplatform.matchers.PositionMatcher

trait StringFunctions { self: PosMatcherFunctions with PartFunctions =>
  def validString(start: Int, end: Int, value: String) = ConstString(StartEndMatch(start, end), validPart(value))
  def validString(pos: PositionMatcher, value: String) = ConstString(pos, validPart(value))
  def validString(value: String)                       = ConstString(AnyPosition, validPart(value))

  def invalidString(start: Int, end: Int, message: String) = ConstString(StartEndMatch(start, end), PartInvalid(AnyPosition, message))
  def invalidString(pos: PositionMatcher, message: String) = ConstString(pos, PartInvalid(AnyPosition, message))
  def invalidString(message: String)                       = ConstString(AnyPosition, PartInvalid(AnyPosition, message))

  def constString(start: Int, end: Int, part: PartMatcher[String]) = ConstString(StartEndMatch(start, end), part)
  def constString(pos: PositionMatcher, part: PartMatcher[String]) = ConstString(pos, part)
  def constString(part: PartMatcher[String])                       = ConstString(AnyPosition, part)
}
