package com.wavesplatform.lang.v1.parser

import com.wavesplatform.lang.v1.parser.BinaryOperations.BINARY_OP_KIND
import scodec.bits.ByteVector

object Expressions {

  case class LET(name: String, value: EXPR)
  sealed trait EXPR
  case class CONST_LONG(value: Long)                               extends EXPR
  case class GETTER(ref: EXPR, field: String)                      extends EXPR
  case class CONST_BYTEVECTOR(value: ByteVector)                   extends EXPR
  case class CONST_STRING(value: String)                           extends EXPR
  case class BINARY_OP(a: EXPR, kind: BINARY_OP_KIND, b: EXPR)     extends EXPR
  case class BLOCK(let: LET, body: EXPR)                           extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)           extends EXPR
  case class REF(key: String)                                      extends EXPR
  case object TRUE                                                 extends EXPR
  case object FALSE                                                extends EXPR
  case class FUNCTION_CALL(functionName: String, args: List[EXPR]) extends EXPR

}
