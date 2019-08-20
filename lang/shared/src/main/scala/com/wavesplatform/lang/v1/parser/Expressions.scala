package com.wavesplatform.lang.v1.parser

import com.wavesplatform.common.state.ByteStr

object Expressions {

  sealed trait Pos {
    def end: Int
    def start: Int
  }

  object Pos {
    def apply(start: Int, end: Int): Pos = RealPos(start, end)

    override def equals(obj: scala.Any): Boolean = super.equals(obj)

    final case class RealPos(start: Int, end: Int) extends Pos { self =>
      override def equals(obj: scala.Any): Boolean = obj match {
        case AnyPos        => true
        case RealPos(s, e) => s == start && e == end
        case _             => false
      }
    }
    case object AnyPos extends Pos {
      override def equals(obj: scala.Any): Boolean = obj match {
        case _: Pos => true
        case _      => false
      }

      override def start: Int = -1
      override def end: Int   = -1
    }
  }

  trait Positioned {
    def position: Pos
  }

  sealed trait PART[+T] extends Positioned
  object PART {
    case class VALID[T](position: Pos, v: T)           extends PART[T] {
      override def equals(obj: Any): Boolean =
        obj match {
          case VALID(_, value) => value.equals(this.v)
          case _               => false
        }

      override def hashCode: Int = v.hashCode
    }
    case class INVALID(position: Pos, message: String) extends PART[Nothing]

    def toEither[T](part: PART[T]): Either[String, T] = part match {
      case Expressions.PART.VALID(_, x)         => Right(x)
      case Expressions.PART.INVALID(p, message) => Left(message)
    }

    def toOption[T](part: PART[T]): Option[T] = part match {
      case Expressions.PART.VALID(_, x)         => Some(x)
      case Expressions.PART.INVALID(p, message) => None
    }
  }

  sealed trait Declaration extends Positioned {
    def position: Pos
    def name: PART[String]
    def allowShadowing: Boolean
  }

  case class LET(position: Pos, name: PART[String], value: EXPR, types: Seq[PART[String]], allowShadowing: Boolean = false) extends Declaration

  type TypeParam = Option[PART[String]]
  type Type      = (PART[String], TypeParam)
  type FuncArgs  = Seq[(PART[String], Seq[Type])]

  case class FUNC(position: Pos, name: PART[String], args: FuncArgs, expr: EXPR) extends Declaration {
    val allowShadowing = false
  }

  case class ANNOTATION(position: Pos, name: PART[String], args: Seq[PART[String]]) extends Positioned
  case class ANNOTATEDFUNC(position: Pos, anns: Seq[ANNOTATION], f: FUNC) extends Positioned {
    def name = f.name
  }
  sealed trait EXPR                                                             extends Positioned
  case class CONST_LONG(position: Pos, value: Long)                             extends EXPR
  case class GETTER(position: Pos, ref: EXPR, field: PART[String])              extends EXPR
  case class CONST_BYTESTR(position: Pos, value: PART[ByteStr])                 extends EXPR
  case class CONST_STRING(position: Pos, value: PART[String])                   extends EXPR
  case class BINARY_OP(position: Pos, a: EXPR, kind: BinaryOperation, b: EXPR)  extends EXPR
  case class BLOCK(position: Pos, let: Declaration, body: EXPR)                 extends EXPR
  case class IF(position: Pos, cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)         extends EXPR
  case class REF(position: Pos, key: PART[String])                              extends EXPR
  case class TRUE(position: Pos)                                                extends EXPR
  case class FALSE(position: Pos)                                               extends EXPR
  case class FUNCTION_CALL(position: Pos, name: PART[String], args: List[EXPR]) extends EXPR
  case class MATCH_CASE(position: Pos, newVarName: Option[PART[String]], types: Seq[PART[String]], expr: EXPR)
  case class MATCH(position: Pos, expr: EXPR, cases: Seq[MATCH_CASE]) extends EXPR

  case class INVALID(position: Pos, message: String) extends EXPR

  case class DAPP(position: Pos, decs: List[Declaration], fs: List[ANNOTATEDFUNC])

  implicit class PartOps[T](val self: PART[T]) extends AnyVal {
    def toEither: Either[String, T] = self match {
      case Expressions.PART.VALID(_, x)         => Right(x)
      case Expressions.PART.INVALID(p, message) => Left(s"Can't compile an invalid instruction: $message in ${p.start}-${p.end}")
    }
  }

}
