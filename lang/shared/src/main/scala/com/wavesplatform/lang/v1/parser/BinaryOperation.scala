package com.wavesplatform.lang.v1.parser

sealed trait BinaryOperation

object BinaryOperation {

  val opsByPriority = List[(String, BinaryOperation)](
    "||" -> OR_OP,
    "&&" -> AND_OP,
    "==" -> EQ_OP,
    ">=" -> GE_OP,
    ">"  -> GT_OP,
    "+"  -> SUM_OP
  )

  val opsToFunctions = opsByPriority.map { case (str, op) => op -> str }.toMap

  case object SUM_OP extends BinaryOperation
  case object AND_OP extends BinaryOperation
  case object OR_OP  extends BinaryOperation
  case object EQ_OP  extends BinaryOperation
  case object GT_OP  extends BinaryOperation
  case object GE_OP  extends BinaryOperation

}
