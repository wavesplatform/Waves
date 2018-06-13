package com.wavesplatform.matchers.functions

import com.wavesplatform.matchers.ExprMatcher.{ConstBytevector, PartMatcher}
import com.wavesplatform.matchers.PositionMatcher.{AnyPosition, StartEndMatch}
import com.wavesplatform.matchers.PositionMatcher
import scodec.bits.ByteVector

trait ByteVectorFunctions { self: PosMatcherFunctions with PartFunctions =>
  def validBV(start: Int, end: Int, value: ByteVector): ConstBytevector = ConstBytevector(StartEndMatch(start, end), validPart(value))
  def validBV(pos: PositionMatcher, value: ByteVector): ConstBytevector = ConstBytevector(pos, validPart(value))
  def validBV(value: ByteVector): ConstBytevector                       = ConstBytevector(AnyPosition, validPart(value))

  def invalidBV(start: Int, end: Int, message: String): ConstBytevector = ConstBytevector(StartEndMatch(start, end), invalidPart(message))
  def invalidBV(pos: PositionMatcher, message: String): ConstBytevector = ConstBytevector(pos, invalidPart(message))
  def invalidBV(message: String): ConstBytevector                       = ConstBytevector(AnyPosition, invalidPart(message))

  def constBV(start: Int, end: Int, part: PartMatcher[ByteVector]): ConstBytevector = ConstBytevector(StartEndMatch(start, end), part)
  def constBV(pos: PositionMatcher, part: PartMatcher[ByteVector]): ConstBytevector = ConstBytevector(pos, part)
  def constBV(part: PartMatcher[ByteVector]): ConstBytevector                       = ConstBytevector(AnyPosition, part)
}
