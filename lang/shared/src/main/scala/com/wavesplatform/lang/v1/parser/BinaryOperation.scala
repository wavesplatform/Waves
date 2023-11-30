package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions.*
import fastparse.*

sealed abstract class BinaryOperation {
  val func: String
  def parser[A: P]: P[BinaryOperation] = P(func).map(_ => this)
  def expr(shiftedStart: Int, shiftedEnd: Int, op1: EXPR, op2: EXPR): EXPR = {
    BINARY_OP(Pos.fromShifted(shiftedStart, shiftedEnd), op1, this, op2)
  }
}

object BinaryOperation {

  implicit def hack(p: fastparse.P[Any]): fastparse.P[Unit] = p.map(_ => ())

  // No monadic notion here, Left and Right mean `left-associative and `right-associative`
  val opsByPriority: List[Either[List[BinaryOperation], List[BinaryOperation]]] = List(
    Left(List(OR_OP)),
    Left(List(AND_OP)),
    Left(List(GT_OP, GE_OP, LT_OP, LE_OP)),
    Left(List(EQ_OP, NE_OP)),
    Right(List(CONS_OP)),
    Left(List(SUM_OP, SUB_OP)),
    Left(List(MUL_OP, DIV_OP, MOD_OP)),
    Left(List(LIST_APPEND_OP, LIST_CONCAT_OP))
  )

  def opsToFunctions(op: BinaryOperation): String = op.func

  case object OR_OP extends BinaryOperation {
    val func = "||"
  }
  case object AND_OP extends BinaryOperation {
    val func = "&&"
  }
  case object EQ_OP extends BinaryOperation {
    val func = "=="
  }
  case object NE_OP extends BinaryOperation {
    val func = "!="
  }
  case object GE_OP extends BinaryOperation {
    val func = ">="
  }
  case object GT_OP extends BinaryOperation {
    val func                  = ">"
    override def parser[A: P]: P[BinaryOperation] = P(">" ~ !P("=")).map(_ => this)
  }
  case object SUM_OP extends BinaryOperation {
    val func = "+"
  }
  case object SUB_OP extends BinaryOperation {
    val func = "-"
  }
  case object MUL_OP extends BinaryOperation {
    override val func: String = "*"
  }
  case object DIV_OP extends BinaryOperation {
    override val func: String = "/"
  }
  case object MOD_OP extends BinaryOperation {
    override val func: String = "%"
  }
  case object LE_OP extends BinaryOperation {
    val func                                      = ">="
    override def parser[A: P]: P[BinaryOperation] = P("<=").map(_ => this)
    override def expr(shiftedStart: Int, shiftedEnd: Int, op1: EXPR, op2: EXPR): EXPR = {
      BINARY_OP(Pos.fromShifted(shiftedStart, shiftedEnd), op2, LE_OP, op1)
    }
  }
  case object LT_OP extends BinaryOperation {
    val func                                      = ">"
    override def parser[A: P]: P[BinaryOperation] = P("<" ~ !P("=")).map(_ => this)
    override def expr(shiftedStart: Int, shiftedEnd: Int, op1: EXPR, op2: EXPR): EXPR = {
      BINARY_OP(Pos.fromShifted(shiftedStart, shiftedEnd), op2, LT_OP, op1)
    }
  }
  case object CONS_OP extends BinaryOperation {
    override val func: String = "::"
    override def expr(shiftedStart: Int, shiftedEnd: Int, op1: EXPR, op2: EXPR): EXPR = {
      val pos = Pos.fromShifted(shiftedStart, shiftedEnd)
      FUNCTION_CALL(pos, PART.VALID(pos, "cons"), List(op1, op2))
    }
  }

  case object LIST_APPEND_OP extends BinaryOperation {
    override val func: String = ":+"
  }

  case object LIST_CONCAT_OP extends BinaryOperation {
    override val func: String = "++"
  }
}
