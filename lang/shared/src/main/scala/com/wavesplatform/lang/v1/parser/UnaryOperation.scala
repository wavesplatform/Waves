package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions._
import fastparse.all._

sealed abstract class UnaryOperation {
  val parser: P[Any]
  def expr(start: Int, end: Int, op: EXPR): EXPR
}

object UnaryOperation {

  val unaryOps: List[UnaryOperation] = List(
    NEGATIVE_OP,
    NOT_OP
  )

  case object NEGATIVE_OP extends UnaryOperation {
    override val parser: P[Any] = P("-" ~ !CharIn('0' to '9'))
    override def expr(start: Int, end: Int, op: EXPR): EXPR = {
      FUNCTION_CALL(Pos(start, end), PART.VALID(Pos(start, end), "-"), List(op))
    }
  }

  case object NOT_OP extends UnaryOperation {
    override val parser: P[Any] = P("!")
    override def expr(start: Int, end: Int, op: EXPR): EXPR = {
      FUNCTION_CALL(Pos(start, end), PART.VALID(Pos(start, end), "!"), List(op))
    }
  }

}
