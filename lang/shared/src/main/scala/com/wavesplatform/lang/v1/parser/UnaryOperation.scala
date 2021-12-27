package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions._
import fastparse._

sealed abstract class UnaryOperation {
  val func: String
  def parser[A: P]: P[Any]
  def expr(start: Int, end: Int, op: EXPR): EXPR
}

object UnaryOperation {

  implicit def hack(p: fastparse.P[Any]): fastparse.P[Unit] = p.map(_ => ())

  val unaryOps: List[UnaryOperation] = List(
    NEGATIVE_OP,
    NOT_OP
  )

  case object POSITIVE_OP extends UnaryOperation {
    val func = "+"
    override def parser[A: P]: P[Any] = P("+" ~ !CharIn("0-9"))
    override def expr(start: Int, end: Int, op: EXPR): EXPR = {
      FUNCTION_CALL(Pos(start, end), PART.VALID(Pos(start, end), "+"), List(op))
    }
  }

  case object NEGATIVE_OP extends UnaryOperation {
    val func = "-"
    override def parser[A: P]: P[Any] = P("-" ~ !CharIn("0-9"))
    override def expr(start: Int, end: Int, op: EXPR): EXPR = {
      FUNCTION_CALL(Pos(start, end), PART.VALID(Pos(start, end), "-"), List(op))
    }
  }

  case object NOT_OP extends UnaryOperation {
    val func = "!"
    override def parser[A: P]: P[Any] = P("!")
    override def expr(start: Int, end: Int, op: EXPR): EXPR = {
      FUNCTION_CALL(Pos(start, end), PART.VALID(Pos(start, end), "!"), List(op))
    }
  }

}
