package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.FunctionHeader
import scodec.bits.ByteVector

object Terms {
  sealed abstract class EXPR
  case class LET(name: String, value: EXPR)
  case class CONST_LONG(t: Long)                                       extends EXPR
  case class GETTER(expr: EXPR, field: String)                         extends EXPR
  case class CONST_BYTEVECTOR(bs: ByteVector)                          extends EXPR
  case class CONST_STRING(s: String)                                   extends EXPR
  case class BLOCK(let: LET, body: EXPR)                               extends EXPR
  case class IF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR)               extends EXPR
  case class REF(key: String)                                          extends EXPR
  case object TRUE                                                     extends EXPR
  case object FALSE                                                    extends EXPR
  case class FUNCTION_CALL(function: FunctionHeader, args: List[EXPR]) extends EXPR
}
