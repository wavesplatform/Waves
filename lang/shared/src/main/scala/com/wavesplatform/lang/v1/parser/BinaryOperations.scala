package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.ctx.PredefFunction
import com.wavesplatform.lang.v1.parser.Terms._
import scodec.bits.ByteVector

object BinaryOperations {

  val opsByPriority = List[(String, BINARY_OP_KIND)](
    "||" -> OR_OP,
    "&&" -> AND_OP,
    "==" -> EQ_OP,
    ">=" -> GE_OP,
    ">"  -> GT_OP,
    "+"  -> SUM_OP
  )

  val opsToFunctions = opsByPriority.map { case (str, op) => op -> str }.toMap

  sealed trait BINARY_OP_KIND
  case object SUM_OP extends BINARY_OP_KIND
  case object AND_OP extends BINARY_OP_KIND
  case object OR_OP  extends BINARY_OP_KIND
  case object EQ_OP  extends BINARY_OP_KIND
  case object GT_OP  extends BINARY_OP_KIND
  case object GE_OP  extends BINARY_OP_KIND

}
