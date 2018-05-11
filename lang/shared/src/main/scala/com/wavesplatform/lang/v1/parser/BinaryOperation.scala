package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.Expressions._
import fastparse.all._

sealed abstract class BinaryOperation {
  val func: String
  val parser: P[Any] = P(func)
  def expr(op1: EXPR)(op2: EXPR): EXPR = {
    BINARY_OP(op1, this, op2)
  }
}

object BinaryOperation {

  val opsByPriority: List[BinaryOperation] = List[BinaryOperation](
    OR_OP,
    AND_OP,
    EQ_OP,
    NE_OP,
    GT_OP,
    GE_OP,
    LT_OP,
    LE_OP,
    SUM_OP,
    SUB_OP
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
  }
  case object SUM_OP extends BinaryOperation {
    val func = "+"
  }
  case object SUB_OP extends BinaryOperation {
    val func = "-"
  }
  case object LE_OP extends BinaryOperation {
    val func            = ">="
    override val parser = P("<=")
    override def expr(op1: EXPR)(op2: EXPR): EXPR = {
      BINARY_OP(op2, LE_OP, op1)
    }
  }
  case object LT_OP extends BinaryOperation {
    val func            = ">"
    override val parser = P("<")
    override def expr(op1: EXPR)(op2: EXPR): EXPR = {
      BINARY_OP(op2, LT_OP, op1)
    }
  }

}
