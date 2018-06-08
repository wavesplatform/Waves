package com.wavesplatform.matchers

import com.wavesplatform.lang.v1.parser.Expressions._
import com.wavesplatform.lang.v1.parser.{BinaryOperation, Expressions}
import org.scalatest.matchers._
import scodec.bits.ByteVector

import scala.reflect.runtime.universe._

sealed trait ExprMatcher extends BeMatcher[Expressions.EXPR]

object ExprMatcher {

  case object AnyExpr extends ExprMatcher {
    override def apply(left: EXPR): MatchResult = MatchResult(
      true,
      "",
      ""
    )
  }

  sealed trait PartMatcher[T] extends BeMatcher[PART[T]]

  case class AnyPart[T](positionMatcher: PositionMatcher) extends PartMatcher[T] {
    override def apply(left: PART[T]): MatchResult = left match {
      case PART.VALID(s, e, _)   => positionMatcher((s, e))
      case PART.INVALID(s, e, _) => positionMatcher((s, e))
    }
  }
  case class PartValid[T: TypeTag](positionMatcher: PositionMatcher, value: T) extends PartMatcher[T] {
    override def apply(left: PART[T]): MatchResult = left match {
      case PART.VALID(s, e, v: T) =>
        positionMatcher((s, e)) <|>
          MatchResult(
            v == value,
            s"get ${v}, while expecting ${value}",
            s"get unexpected ${value}",
            IndexedSeq(v, value)
          )
      case _ => wrongTypeMatchResult[left.type, PART.VALID[T]]
    }
  }
  case class PartInvalid[T: TypeTag](positionMatcher: PositionMatcher, message: String) extends PartMatcher[T] {
    override def apply(left: PART[T]): MatchResult = left match {
      case PART.INVALID(s, e, m) =>
        positionMatcher((s, e)) <|>
          MatchResult(
            m equals message,
            s"$m doesnot match $message",
            s"$m unexpectly match $message"
          )
      case _ => wrongTypeMatchResult[left.type, PART.INVALID]
    }
  }

  case class Let(positionMatcher: PositionMatcher, name: PartMatcher[String], value: ExprMatcher) extends Matcher[LET] {
    override def apply(left: LET): MatchResult = {
      positionMatcher((left.start, left.end)) <|>
        name(left.name) <|>
        value(left.value)
    }
  }

  case class ConstLong(positionMatcher: PositionMatcher, value: Long) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case CONST_LONG(s, e, l) =>
        positionMatcher((s, e)) <|>
          MatchResult(
            l == value,
            s"get ${l} while expecting $value",
            s"get unexpected $value"
          )
      case _ => wrongTypeMatchResult[left.type, CONST_LONG]
    }
  }

  case class ConstString(positionMatcher: PositionMatcher, partMatcher: PartMatcher[String]) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case CONST_STRING(s, e, v) => positionedMatcher(s, e, positionMatcher, partMatcher)(v)
      case _                     => wrongTypeMatchResult[left.type, CONST_STRING]
    }
  }

  case class ConstBytevector(offsetMatcher: PositionMatcher, partMatcher: PartMatcher[ByteVector]) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case CONST_BYTEVECTOR(s, e, v) => positionedMatcher(s, e, offsetMatcher, partMatcher)(v)
      case _                         => wrongTypeMatchResult[left.type, CONST_BYTEVECTOR]
    }
  }

  case class Getter(offsetMatcher: PositionMatcher, ref: ExprMatcher, field: PartMatcher[String]) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case GETTER(s, e, r, f) => offsetMatcher((s, e)) <|> ref(r) <|> field(f)
      case _                  => wrongTypeMatchResult[left.type, GETTER]
    }
  }

  case class BinaryOp(positionMatcher: PositionMatcher, left: ExprMatcher, kind: BinaryOperation, right: ExprMatcher) extends ExprMatcher {
    override def apply(expr: Expressions.EXPR): MatchResult = expr match {
      case BINARY_OP(s, e, a, k, b) =>
        positionMatcher((s, e)) <|>
          MatchResult(k == kind, s"got $k while expecting $kind", s"got unexpected $kind", IndexedSeq(k, kind)) <|>
          left(a) <|>
          right(b)
      case _ => wrongTypeMatchResult[expr.type, BINARY_OP]
    }
  }
  case class Block(positionMatcher: PositionMatcher, let: Matcher[LET], body: ExprMatcher) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case BLOCK(s, e, l, b) =>
        positionMatcher((s, e)) <|>
          let(l) <|>
          body(b)
      case _ => wrongTypeMatchResult[left.type, BLOCK]
    }
  }
  case class If(positionMatcher: PositionMatcher, cond: ExprMatcher, ifTrue: ExprMatcher, ifFalse: ExprMatcher) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case IF(s, e, con, itr, ifa) =>
        positionMatcher((s, e)) <|>
          cond(con) <|>
          ifTrue(itr) <|>
          ifFalse(ifa)

      case _ => wrongTypeMatchResult[left.type, IF]
    }
  }
  case class Ref(positionMatcher: PositionMatcher, key: PartMatcher[String]) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case REF(s, e, k) => positionMatcher((s, e)) <|> key(k)
      case _            => wrongTypeMatchResult[left.type, REF]
    }
  }
  case class True(positionMatcher: PositionMatcher) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case TRUE(s, e) => positionMatcher((s, e))
      case _          => wrongTypeMatchResult[left.type, TRUE]
    }
  }
  case class False(positionMatcher: PositionMatcher) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case FALSE(s, e) => positionMatcher((s, e))
      case _           => wrongTypeMatchResult[left.type, FALSE]
    }
  }
  //TODO: add args matchers
  case class FunctionCall(positionMatcher: PositionMatcher, name: PartMatcher[String], argMatcher: Matcher[List[EXPR]]) extends ExprMatcher {
    override def apply(left: Expressions.EXPR): MatchResult = left match {
      case FUNCTION_CALL(s, e, n, _) => positionMatcher((s, e)) <|> name(n)
      case _                         => wrongTypeMatchResult[left.type, FUNCTION_CALL]
    }
  }
}
