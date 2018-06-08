package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers.ExprMatcher.{PartMatcher, Ref}
import com.wavesplatform.matchers.PositionMatcher.StartEndMatch
import com.wavesplatform.matchers.{ExprMatcher, PositionMatcher}

trait RefFunctions { self: PosMatcherFunctions with PartFunctions =>
  def validRef(key: String): ExprMatcher = Ref(anyPos, validPart(key))
  def validRef(start: Int, end: Int, key: String): ExprMatcher = Ref(StartEndMatch(start, end), validPart(key))
  def validRef(positionMatcher: PositionMatcher, key: String): ExprMatcher = Ref(positionMatcher, validPart(key))

  def invalidRef(msg: String): ExprMatcher = Ref(anyPos, invalidPart(msg))
  def invalidRef(start: Int, end: Int, msg: String): ExprMatcher = Ref(StartEndMatch(start, end), invalidPart(msg))
  def invalidRef(positionMatcher: PositionMatcher, msg: String): ExprMatcher = Ref(positionMatcher, invalidPart(msg))

  def ref(key: PartMatcher[String]): ExprMatcher = Ref(anyPos, key)
  def ref(start: Int, end: Int, key: PartMatcher[String]): ExprMatcher = Ref(StartEndMatch(start, end), key)
  def ref(positionMatcher: PositionMatcher, key: PartMatcher[String]): ExprMatcher = Ref(positionMatcher, key)
}
