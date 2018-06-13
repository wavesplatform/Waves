package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions._
import fastparse.all._

sealed abstract class BinaryOperation {
  val func: String
  val parser: P[BinaryOperation] = P(func).map(_ => this)
  def expr(start: Int, end: Int, op1: EXPR, op2: EXPR): EXPR = {
    BINARY_OP(Pos(start, end), op1, this, op2)
  }
}

object BinaryOperation {

  val opsByPriority: List[List[BinaryOperation]] = List(
    List(OR_OP, AND_OP),
    List(EQ_OP, NE_OP),
    List(GT_OP, GE_OP, LT_OP, LE_OP),
    List(SUM_OP, SUB_OP),
    List(MUL_OP, DIV_OP, MOD_OP)
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
    val func = ">"
    override val parser = P(">" ~ !P("=")).map(_ => this)
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
    val func            = ">="
    override val parser = P("<=").map(_ => this)
    override def expr(start: Int, end: Int, op1: EXPR, op2: EXPR): EXPR = {
      BINARY_OP(Pos(start, end), op2, LE_OP, op1)
    }
  }
  case object LT_OP extends BinaryOperation {
    val func            = ">"
    override val parser = P("<" ~ !P("=")).map(_ => this)
    override def expr(start: Int, end: Int, op1: EXPR, op2: EXPR): EXPR = {
      BINARY_OP(Pos(start, end), op2, LT_OP, op1)
    }
  }

}
