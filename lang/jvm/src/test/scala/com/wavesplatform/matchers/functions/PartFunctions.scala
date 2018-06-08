package com.wavesplatform.matchers.functions

import com.wavesplatform.lang.v1.parser.Expressions.PART
import com.wavesplatform.matchers.ExprMatcher.{PartInvalid, PartMatcher, PartValid}
import com.wavesplatform.matchers.PositionMatcher.{AnyPosition, StartEndMatch}
import com.wavesplatform.matchers.{PositionMatcher, any}
import org.scalatest.matchers.BeMatcher

import scala.reflect.runtime.universe._

trait PartFunctions { self: PosMatcherFunctions =>
  def anyPart[T: TypeTag]: BeMatcher[PART[T]] = any[PART[T]]

  def validPart[T: TypeTag](start: Int, end: Int, value: T): PartMatcher[T] = PartValid(StartEndMatch(start, end), value)
  def validPart[T: TypeTag](pos: PositionMatcher, value: T): PartMatcher[T] = PartValid(pos, value)
  def validPart[T: TypeTag](value: T): PartMatcher[T]                       = PartValid(anyPos, value)

  def invalidPart[T: TypeTag](start: Int, end: Int, message: String): PartMatcher[T] = PartInvalid[T](StartEndMatch(start, end), message)
  def invalidPart[T: TypeTag](pos: PositionMatcher, message: String): PartMatcher[T] = PartInvalid[T](pos, message)
  def invalidPart[T: TypeTag](message: String): PartMatcher[T]                       = PartInvalid[T](AnyPosition, message)
}
