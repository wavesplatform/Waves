package com.wavesplatform.lang.v1.parser

import scodec.bits.ByteVector

object Expressions {

  trait Positioned {
    def start: Int
    def end: Int
  }

  sealed trait PART[+T] extends Positioned
  object PART {
    case class VALID[T](start: Int, end: Int, v: T)           extends PART[T]
    case class INVALID(start: Int, end: Int, message: String) extends PART[Nothing]
  }

  case class LET(start: Int, end: Int, name: PART[String], value: EXPR, types: Seq[PART[String]]) extends Positioned

  sealed trait EXPR                                                                   extends Positioned
  case class CONST_LONG(start: Int, end: Int, value: Long)                            extends EXPR
  case class GETTER(start: Int, end: Int, ref: EXPR, field: PART[String])             extends EXPR
  case class CONST_BYTEVECTOR(start: Int, end: Int, value: PART[ByteVector])          extends EXPR
  case class CONST_STRING(start: Int, end: Int, value: PART[String])                  extends EXPR
  case class BINARY_OP(start: Int, end: Int, a: EXPR, kind: BinaryOperation, b: EXPR) extends EXPR
  case class BLOCK(start: Int, end: Int, let: LET, body: EXPR)                        extends EXPR
  case class IF(start: Int, end: Int, cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)        extends EXPR
  case class REF(start: Int, end: Int, key: PART[String])                             extends EXPR

  case class TRUE(start: Int, end: Int)  extends EXPR
  case class FALSE(start: Int, end: Int) extends EXPR

  case class FUNCTION_CALL(start: Int, end: Int, name: PART[String], args: List[EXPR]) extends EXPR

  case class MATCH_CASE(start: Int, end: Int, newVarName: Option[PART[String]], types: Seq[PART[String]], expr: EXPR)
  case class MATCH(start: Int, end: Int, expr: EXPR, cases: Seq[MATCH_CASE]) extends EXPR

  case class INVALID(start: Int, end: Int, message: String, next: Option[EXPR] = None) extends EXPR
  implicit class PartOps[T](val self: PART[T]) extends AnyVal {
    def toEither: Either[String, T] = self match {
      case Expressions.PART.VALID(_, _, x)         => Right(x)
      case Expressions.PART.INVALID(_, _, message) => Left(message)
    }
  }

}
