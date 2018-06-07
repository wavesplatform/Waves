package com.wavesplatform

import com.wavesplatform.lang.v1.parser.Expressions.EXPR
import com.wavesplatform.matchers.ExprMatcher._
import com.wavesplatform.matchers.PositionMatcher.{AnyPosition, EndMatch, StartEndMatch, StartMatch}
import org.scalatest.matchers.{MatchResult, Matcher}
import scodec.bits.ByteVector

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

  def positionedMatcher[A](start: Int, end: Int, pos: PositionMatcher, matcher: Matcher[A])(a: A): MatchResult =
    pos((start, end)) <|> matcher(a)

  val anyExpr: ExprMatcher = AnyExpr

  /***
    *
    * POSITIONS
    *
    */
  val **                = AnyPosition
  def startsFrom(s: Int) = StartMatch(s)
  def endsAt(e: Int)     = EndMatch(e)

  /***
    *
    * PARTS
    *
    */
  def anyPart[T: TypeTag]                                   = AnyPart[T](**)
//  val anyValidPart                                 = ???
  def validPart[T: TypeTag](start: Int, end: Int, value: T) = PartValid(StartEndMatch(start, end), value)
  def validPart[T: TypeTag](pos: PositionMatcher, value: T) = PartValid(pos, value)
  def validPart[T: TypeTag](value: T)                       = PartValid(**, value)

//  val anyInvalidPart                                        = ???
  def invalidPart[T: TypeTag](start: Int, end: Int, message: String) = PartInvalid[T](StartEndMatch(start, end), message)
  def invalidPart[T: TypeTag](pos: PositionMatcher, message: String) = PartInvalid[T](pos, message)
  def invalidPart[T: TypeTag](message: String)                       = PartInvalid[T](AnyPosition, message)

  /***
    *
    * BOOLS
    *
    */
  val `true`  = True(**)
  val `false` = False(**)

  /***
    *
    * LONG FUNCTIONS
    *
    */
//  val anyLong: ExprMatcher                         = ???
  def constLong(start: Int, end: Int, value: Long) = ConstLong(StartEndMatch(start, end), value)
  def constLong(pos: PositionMatcher, value: Long) = ConstLong(pos, value)
  def constLong(value: Long)                       = ConstLong(AnyPosition, value)

  /***
    *
    * STRING FUNCTIONS
    *
    */
//  val anyValidString                                   = ???
//  val anyInvalidString                                 = ???
  def validString(start: Int, end: Int, value: String) = ConstString(StartEndMatch(start, end), validPart(value))
  def validString(pos: PositionMatcher, value: String) = ConstString(pos, validPart(value))
  def validString(value: String)                       = ConstString(AnyPosition, validPart(value))

  def invalidString(start: Int, end: Int, message: String) = ConstString(StartEndMatch(start, end), PartInvalid(AnyPosition, message))
  def invalidString(pos: PositionMatcher, message: String) = ConstString(pos, PartInvalid(AnyPosition, message))
  def invalidString(message: String)                       = ConstString(AnyPosition, PartInvalid(AnyPosition, message))

  def constString(start: Int, end: Int, part: PartMatcher[String]) = ConstString(StartEndMatch(start, end), part)
  def constString(pos: PositionMatcher, part: PartMatcher[String]) = ConstString(pos, part)
  def constString(part: PartMatcher[String])                       = ConstString(AnyPosition, part)

  /***
    *
    * ByteVector Functions
    *
    */
//  val anyValidBV                                       = ???
//  val anyinvalidBV                                     = ???
  def validBV(start: Int, end: Int, value: ByteVector) = ConstBytevector(StartEndMatch(start, end), validPart(value))
  def validBV(pos: PositionMatcher, value: ByteVector) = ConstBytevector(pos, validPart(value))
  def validBV(value: ByteVector)                       = ConstBytevector(AnyPosition, validPart(value))

  def invalidBV(start: Int, end: Int, message: String) = ConstBytevector(StartEndMatch(start, end), invalidPart(message))
  def invalidBV(pos: PositionMatcher, message: String) = ConstBytevector(pos, invalidPart(message))
  def invalidBV(message: String)                       = ConstBytevector(AnyPosition, invalidPart(message))

  def constBV(start: Int, end: Int, part: PartMatcher[ByteVector]) = ConstBytevector(StartEndMatch(start, end), part)
  def constBV(pos: PositionMatcher, part: PartMatcher[ByteVector]) = ConstBytevector(pos, part)
  def constBV(part: PartMatcher[ByteVector])                       = ConstBytevector(AnyPosition, part)

  /***
    *
    * GETTER Functions
    *
    */
  val anyGetter = Getter(AnyPosition, anyExpr, anyPart)

}
