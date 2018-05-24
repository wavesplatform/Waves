package com.wavesplatform.lang.v1.parser

import scodec.bits.ByteVector

object Expressions {

  sealed trait PART[+T]
  object PART {
    case class VALID[T](v: T)                             extends PART[T]
    case class INVALID(consumed: String, message: String) extends PART[Nothing]
  }

  case class LET(name: PART[String], value: EXPR, types: Seq[PART[String]])
  object LET {
    def apply(name: String, value: EXPR, types: Seq[String]): LET = LET(PART.VALID(name), value, types.map(PART.VALID(_)))
  }

  sealed trait EXPR
  case class CONST_LONG(value: Long) extends EXPR

  case class GETTER(ref: EXPR, field: PART[String]) extends EXPR
  object GETTER {
    def apply(ref: EXPR, field: String): GETTER = GETTER(ref, PART.VALID(field))
  }

  case class CONST_BYTEVECTOR(value: PART[ByteVector]) extends EXPR
  object CONST_BYTEVECTOR {
    def apply(x: ByteVector): CONST_BYTEVECTOR = CONST_BYTEVECTOR(PART.VALID(x))
  }

  case class CONST_STRING(value: PART[String]) extends EXPR
  object CONST_STRING {
    def apply(x: String): CONST_STRING = CONST_STRING(PART.VALID(x))
  }

  case class BINARY_OP(a: EXPR, kind: BinaryOperation, b: EXPR) extends EXPR
  case class BLOCK(let: LET, body: EXPR)                        extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)        extends EXPR

  case class REF(key: PART[String]) extends EXPR
  object REF {
    def apply(key: String): REF = REF(PART.VALID(key))
  }

  case object TRUE  extends EXPR
  case object FALSE extends EXPR

  case class FUNCTION_CALL(name: PART[String], args: List[EXPR]) extends EXPR
  object FUNCTION_CALL {
    def apply(name: String, args: List[EXPR]): FUNCTION_CALL = FUNCTION_CALL(PART.VALID(name), args)
  }

  case class MATCH_CASE(newVarName: Option[PART[String]], types: Seq[PART[String]], expr: EXPR)
  object MATCH_CASE {
    def apply(newVarName: Option[String], types: List[String], expr: EXPR): MATCH_CASE =
      MATCH_CASE(newVarName.map(PART.VALID(_)), types.map(PART.VALID(_)), expr)
  }

  case class MATCH(expr: EXPR, cases: Seq[MATCH_CASE]) extends EXPR

  case class INVALID(message: String, next: Option[EXPR] = None) extends EXPR
  object INVALID {
    def apply(message: String, next: EXPR): INVALID = INVALID(message, Some(next))
  }

  implicit class PartOps[T](val self: PART[T]) extends AnyVal {
    def toEither: Either[String, T] = self match {
      case Expressions.PART.VALID(x)            => Right(x)
      case Expressions.PART.INVALID(x, message) => Left(s"$message: $x")
    }
  }

}
